%% @docs Takes care of an application instance
-module(er_instance_agent).
-behaviour(gen_server).
-include("er_apps.hrl").
-export([start_link/1, destroy/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {id, instance, version, configuration, port, deploy_info, msg_count = 0}).
-record(wrapper, {type = app, trigger_deploy, approve_on_success}).

%% Interface

start_link(Id) -> gen_server:start_link(?MODULE, Id, []).
destroy(Pid) -> gen_server:call(Pid, destroy, 10000).

%% gen_server

init(Id) ->
	er_event_bus:publish({instance_events, Id, {created, "no_version"}}),
	process_flag(trap_exit, true),
	{instance, Instance} = er_apps:get_instance(Id),
	{ok, #state{id = Id, instance = Instance}}.

handle_call(destroy, _From, #state{id = Id, port = Port} = State) ->
	error_logger:info_msg("Stopping and destroying app instance: ~p (~p)~n", [Id, Port]),
	stop_package(State),
	{stop, normal, instance_destroyed, State}.

handle_info({_Port, {data, Line}}, State) ->
	case State of
		#state{msg_count = exceeded} ->
			{noreply, State};
		#state{msg_count = 20} ->
			io:format("~p out: exceeded max output~n", [State#state.id]),
			{noreply, State#state{msg_count = exceeded}};
		#state{msg_count = Count} ->		
			io:format("~p out: ~p~n", [State#state.id, Line]),
			{noreply, State#state{msg_count = Count + 1}}
	end;
handle_info({'EXIT', _Port, _Reason}, State) ->
	{noreply, State};
handle_info({_Port, {exit_status, ExitCode}}, #state{id = Id, version = Version} = State) ->
	io:format("~p: Process exit code: ~p~n", [Id, ExitCode]),
	Outcome = case ExitCode of 0 -> completed; _ -> crashed end,
	er_event_bus:publish({instance_events, Id, {Outcome, Version}}),
	case {Outcome, get_wrapper_configuration(State#state.configuration)} of
		{completed, #wrapper{approve_on_success = {AppName, StageName, Approval}}} ->
			%% TODO: this does indicate a separate responsibility
			{_, _, DeployedVersions} = er_event_bus:get_snapshot(deployed_versions),
			{ok, ApprovedVersionNumber} = orddict:find({AppName, StageName}, DeployedVersions),
			error_logger:info_msg("~p v ~p acquired approval ~p~n", [AppName, ApprovedVersionNumber, Approval]),
			er_repository:approve_version(AppName, ApprovedVersionNumber, Approval);
		_ -> ok
	end,
	{noreply, State#state{port = undefined}}.

handle_cast({deploy, VersionNumber}, #state{port = undefined} = State) ->
	NewState = do_deploy(VersionNumber, State),
	{noreply, NewState};
handle_cast({deploy, VersionNumber}, State) ->
	stop_package(State),
	NewState = do_deploy(VersionNumber, State#state{port = undefined}),
	{noreply, NewState};
handle_cast(start, #state{port = undefined} = State) ->
	Port = start_package(State),
	{noreply, State#state{port = Port}};
handle_cast(start, State) -> {noreply, State};
handle_cast(stop, #state{port = undefined} = State) -> {noreply, State};
handle_cast(stop, State) -> stop_package(State), {noreply, State#state{port = undefined}};
handle_cast({event, deployed_versions, {AppName, StageName}, _Version}, State) ->
	case {State, get_wrapper_configuration(State#state.configuration)} of
		{#state{port = undefined}, #wrapper{trigger_deploy = {AppName, StageName}}} ->
			Port = start_package(State),
			{noreply, State#state{port = Port}};
		_ -> {noreply, State}		
	end;
handle_cast({snapshot, deployed_versions, _}, State) -> {noreply, State}.

terminate(normal, #state{id = Id}) ->
	error_logger:info_msg("~p: Task finnished~n", [Id]),
	ok;
terminate(_, #state{id = Id, port = undefined}) ->
	error_logger:info_msg("~p: Clean-up on shutdown~n", [Id]),
	ok;
terminate(_, #state{id = Id, port = Port} = State) when Port /= undefined ->
	error_logger:info_msg("~p: Clean-up on shutdown (port ~p)~n", [Id, Port]),
	stop_package(State),
	ok.

%% helpers

do_deploy(VersionNumber, #state{id = Id, instance = #instance{app_name = AppName, stage_name = StageName}} = State) ->
	er_event_bus:publish({instance_events, Id, {deploying, VersionNumber}}),
	Configuration = er_apps:effective_configuration(AppName, StageName, Id),
	DeployInfo = deploy(Id, AppName, VersionNumber, Configuration),
	case get_wrapper_configuration(Configuration) of
		#wrapper{type = task, trigger_deploy = TriggerDeploy} -> 
			%% TODO: This should be done in a separate (task-manager) component, to allow for rule processing etc.
			if
				TriggerDeploy /= undefined -> er_event_bus:subscribe(self(), [deployed_versions]);
				true -> ok
			end,
			er_event_bus:publish({instance_events, Id, {ready, VersionNumber}}),
			State#state{id = Id, version = VersionNumber, configuration = Configuration, port = undefined, deploy_info = DeployInfo};
		#wrapper{type = app} ->
			NewState = State#state{id = Id, version = VersionNumber, configuration = Configuration, port = undefined, deploy_info = DeployInfo},
			Port = start_package(NewState),
			error_logger:info_msg("Started ~p with ~p as ~p~n", [VersionNumber, Configuration, Port]),
			NewState#state{port = Port}
	end.

get_env_properties(Id, VersionNumber, Configuration) -> [
		{"VERSION", VersionNumber},
		{"ID", Id}
	] ++ [
		{PropKey, PropValue} || {property, PropKey, PropValue} <- Configuration
	].

deploy(Id, AppName, VersionNumber, Configuration) ->
	Folder = lists:concat([er_configuration:instances_directory(), Id, "-", VersionNumber]),
	error_logger:info_msg("Deploying ~p v ~p in ~p~n", [AppName, VersionNumber, Folder]),
	PackageFile = lists:concat([Folder, "/package.zip"]),
	ok = filelib:ensure_dir(PackageFile),
	ok = er_repository:download_version_file(AppName, VersionNumber, PackageFile),
	{ok, Files} = zip:extract(PackageFile, [{cwd, Folder}]),
	io:format("Files: ~p~n", [Files]),
	ExecFile = lists:concat(["sh run.sh"]),
	% TODO: find one of the allowed executables, instead of assuming run.sh
	Env = get_env_properties(Id, VersionNumber, Configuration),
	{Folder, ExecFile, Env}.

start_package(#state{id = Id, deploy_info = {Folder, ExecFile, Env}} = State) ->
	Port = open_port({spawn, ExecFile}, [stream, {line, 1024}, {cd, Folder}, {env, Env}, exit_status, use_stdio, stderr_to_stdout]),
	er_event_bus:publish({instance_events, Id, {running, State#state.version}}),
	Port.

stop_package(#state{id = Id, port = Port} = State) when Port /= undefined ->
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    os:cmd(io_lib:format("kill -TERM -~p", [OsPid])),
    er_event_bus:publish({instance_events, Id, {stopping, State#state.version}}),
    receive
		{Port, {exit_status, _}} -> er_event_bus:publish({instance_events, Id, {stopped, State#state.version}})
	after
		5000 ->
			os:cmd(io_lib:format("kill -KILL -~p", [OsPid])),
			er_event_bus:publish({instance_events, Id, {force_stopping, State#state.version}}),
			receive
				{Port, {exit_status, _}} -> er_event_bus:publish({instance_events, Id, {force_stopped, State#state.version}})
			after
				10000 -> er_event_bus:publish({instance_events, Id, {force_stop_failed, State#state.version}})
			end
	end.

get_wrapper_configuration(Configuration) -> get_wrapper_configuration(#wrapper{}, Configuration).
get_wrapper_configuration(Wrapper, []) -> Wrapper;
get_wrapper_configuration(Wrapper, [ Entry | Configuration]) ->
	UpdatedWrapper = case Entry of
		{wrapper, type, Type} -> Wrapper#wrapper{type = Type};
		{wrapper, trigger_deploy, {AppName, StageName}} -> Wrapper#wrapper{trigger_deploy = {AppName, StageName}};
		{wrapper, approve_on_success, {AppName, StageName, Approval}} -> Wrapper#wrapper{approve_on_success = {AppName, StageName, Approval}};
		_ -> Wrapper
	end,
	get_wrapper_configuration(UpdatedWrapper, Configuration).

%% other gen_server

code_change(_, _, _) -> stub.
