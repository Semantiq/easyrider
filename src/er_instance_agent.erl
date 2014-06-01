-module(er_instance_agent).
-behaviour(gen_server).
-include("er_repository.hrl").
-export([start_link/3, destroy/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

%% Interface

start_link(Id, Version, Configuration) -> gen_server:start_link(?MODULE, {Id, Version, Configuration}, []).
destroy(Pid) -> gen_server:call(Pid, {destroy}).

%% gen_server

init({Id, Version, Configuration}) ->
	process_flag(trap_exit, true),
	Folder = lists:concat([er_configuration:instances_directory(), Id, "-", Version#version_info.number]),
	io:format("Deploying ~p with ~p in ~p~n", [Version#version_info.number, Configuration, Folder]),
	ok = file:make_dir(Folder),
	PackageFile = lists:concat([Folder, "/package.zip"]),
	ok = er_repository:download_version_file(Version, PackageFile),
	{ok, Files} = zip:extract(PackageFile, [{cwd, Folder}]),
	io:format("Files: ~p~n", [Files]),
	ExecFile = lists:concat(["sh run.sh"]),
	% TODO: find one of the allowed executables, instead of assuming run.sh
	Port = open_port({spawn, ExecFile}, [stream, {line, 1024}, {cd, Folder}]),
	io:format("Starting ~p with ~p as ~p in ~p~n", [Version, Configuration, Port, Folder]),
	{ok, {Id, Version, Configuration, Port}}.

handle_call({destroy}, _From, {Id, Version, Configuration, Port}) ->
	io:format("Stopping and destroying app instance: ~p (~p)~n", [Id, Port]),
	{stop, normal, {destroyed}, {Id, Version, Configuration, Port}};
handle_call({start}, _From, State) -> {reply, {instance_started}, State};
handle_call({stop}, _From, State) -> {reply, {instance_stopped}, State}.

handle_info({'EXIT', _Port, Reason}, {Id, _, _, _}) ->
	io:format("~p: App terminated: ~p~n", [Id, Reason]),
	{stop, normal, undefined}.

handle_cast(_Message, State) -> {noreply, State}.

terminate(normal, undefined) -> ok;
terminate(_, {Id, _, Port, _}) ->
	io:format("~p: Clean-up on shutdown~n", [Id]),
	port_close(Port),
	ok.

%% other gen_server

code_change(_, _, _) -> stub.
