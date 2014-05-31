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
	Folder = lists:concat([er_configuration:instances_directory(), Id, "-", Version#version_info.number]),
	ok = file:make_dir(Folder),
	Pid = erlang:make_ref(),
	io:format("Starting ~p with ~p as ~p in ~p~n", [Version, Configuration, Pid, Folder]),
	{ok, {Version, Configuration, Pid}}.

handle_call({destroy}, _From, {Id, _, _, Pid}) ->
	io:format("Stopping and destroying app instance: ~p (~p)~n", [Id, Pid]),
	{stop, normal, {destroyed}, undefined};
handle_call({start}, _From, State) -> {reply, {instance_started}, State};
handle_call({stop}, _From, State) -> {reply, {instance_stopped}, State}.

handle_cast(_Message, State) -> {noreply, State}.

terminate(normal, _) -> ok.

%% other gen_server

code_change(_, _, _) -> stub.
handle_info(_, _) -> stub.
