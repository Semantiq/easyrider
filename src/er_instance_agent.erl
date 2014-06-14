-module(er_instance_agent).
-behaviour(gen_server).
-include("er_repository.hrl").
-export([start_link/3, destroy/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

%% TODO: use state record
-record(wrapper, {type = app, trigger_app = undefined, trigger_stage = undefined, trigger_delay = undefined}).

%% Interface

start_link(Id, Version, Configuration) -> gen_server:start_link(?MODULE, {Id, Version, Configuration}, []).
destroy(Pid) -> gen_server:call(Pid, {destroy}).

%% gen_server

init({Id, Version, Configuration}) ->
	er_event_bus:publish({instance_events, Id, {deploying, Version}}),
	process_flag(trap_exit, true),
	DeployInfo = deploy(Id, Version, Configuration),
	case get_wrapper_configuration(Configuration) of
		#wrapper{type = task} -> 
			er_event_bus:subscribe(self(), [deployed_versions]),
			er_event_bus:publish({instance_events, Id, {ready, Version}}),
			{ok, {Id, Version, Configuration, undefined, DeployInfo}};
		#wrapper{type = app} ->
			Port = start_package(DeployInfo),
			io:format("Starting ~p with ~p as ~p in ~p~n", [Version, Configuration, Port, DeployInfo]),
			er_event_bus:publish({instance_events, Id, {running, Version}}),
			{ok, {Id, Version, Configuration, Port, DeployInfo}}
	end.

handle_call({destroy}, _From, {Id, Version, Configuration, Port, DeployInfo}) ->
	io:format("Stopping and destroying app instance: ~p (~p)~n", [Id, Port]),
	er_event_bus:publish({instance_events, Id, {stopped, Version}}),
	{stop, normal, {destroyed}, {Id, Version, Configuration, Port, DeployInfo}};
handle_call({start}, _From, State) -> {reply, {instance_started}, State};
handle_call({stop}, _From, State) -> {reply, {instance_stopped}, State}.

handle_info({'EXIT', _Port, Reason}, State) ->
	{Id, _, _, _, _} = State,
	io:format("~p: App terminated: ~p~n", [Id, Reason]),
	{stop, normal, State}.

handle_cast({event, instance_events, {AppName, StageName}, Version}, {Id, Version, Configuration, undefined, DeployInfo}) ->
	% #wrapper{trigger_app = ThisAppName, trigger_stage = ThisAppName} = get_wrapper_configuration(C)
	% if
	% 	AppName ->
	% 		body
	% end
	% Port = start_package(DeployInfo),
	% er_event_bus:publish({instance_events, Id, {running, Version}}),
	{noreply, {Id, Version, Configuration, undefined, DeployInfo}};
handle_cast({snapshot, instance_events, _, _}, State) -> {noreply, State}.

terminate(normal, {Id, Version, _, _, _}) ->
	io:format("~p: Task finnished~n", [Id]),
	er_event_bus:publish({instance_events, Id, {completed, Version}}),
	ok;
terminate(_, {Id, _, _, Port, _}) ->
	io:format("~p: Clean-up on shutdown (port ~p)~n", [Id, Port]),
	port_close(Port),
	ok.

%% helpers

get_env_properties(Id, Version, Configuration) -> [
		{"VERSION", Version#version_info.number},
		{"ID", Id}
	] ++ [
		{PropKey, PropValue} || {property, PropKey, PropValue} <- Configuration
	].

deploy(Id, Version, Configuration) ->
	Folder = lists:concat([er_configuration:instances_directory(), Id, "-", Version#version_info.number]),
	io:format("Deploying ~p with ~p in ~p~n", [Version#version_info.number, Configuration, Folder]),
	ok = filelib:ensure_dir(er_configuration:instances_directory()),
	ok = file:make_dir(Folder),
	PackageFile = lists:concat([Folder, "/package.zip"]),
	ok = er_repository:download_version_file(Version, PackageFile),
	{ok, Files} = zip:extract(PackageFile, [{cwd, Folder}]),
	io:format("Files: ~p~n", [Files]),
	ExecFile = lists:concat(["sh run.sh"]),
	% TODO: find one of the allowed executables, instead of assuming run.sh
	Env = get_env_properties(Id, Version, Configuration),
	io:format("Using env: ~p~n", [Env]),
	{Folder, ExecFile, Env}.

start_package({Folder, ExecFile, Env}) ->
	open_port({spawn, ExecFile}, [stream, {line, 1024}, {cd, Folder}, {env, Env}]).

get_wrapper_configuration(Configuration) -> get_wrapper_configuration(#wrapper{}, Configuration).
get_wrapper_configuration(Wrapper, []) -> Wrapper;
get_wrapper_configuration(Wrapper, [ Entry | Configuration]) ->
	UpdatedWrapper = case Entry of
		{wrapper, type, Type} -> Wrapper#wrapper{type = Type};
		{wrapper, trigger_app, AppName} -> Wrapper#wrapper{trigger_app = AppName};
		{wrapper, trigger_stage, StageName} -> Wrapper#wrapper{trigger_stage = StageName};
		_ -> Wrapper
	end,
	get_wrapper_configuration(UpdatedWrapper, Configuration).

%% other gen_server

code_change(_, _, _) -> stub.
