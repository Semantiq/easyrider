-module(er_orchestrator).
-behaviour(gen_server).
-include("er_apps.hrl").
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

%% Interface

% @private
start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% gen_server

init(_Args) ->
	er_event_bus:subscribe(self(), [recommended_versions]),
	{ok, {}}.

handle_call(_Message, _From, State) -> {reply, ok, State}.

handle_cast({event, recommended_versions, {AppName, StageName}, {Version, immediate}}, State) ->
	io:format("Performing recommened deployment: ~p v ~p to ~p~n", [AppName, Version, StageName]),
	{snapshot, instances, AllInstances} = er_event_bus:get_snapshot(instances),
	Instances = [ Instance || {{ThisAppName, ThisStageName, _Id}, Instance} <- AllInstances, ThisAppName == AppName, ThisStageName == StageName ],
	[ er_node_agent:deploy_instance(Node, Id, Version, er_apps:effective_configuration(AppName, StageName, Id)) || #instance{id = Id, node = Node} <- Instances ],
	er_event_bus:publish({deployed_versions, {AppName, StageName}, Version}),
	{noreply, State};
handle_cast({snapshot, recommended_versions, _Data}, State) ->
	{noreply, State}.

%% other gen_server

terminate(_, _) -> stub.
code_change(_, _, _) -> stub.
handle_info(_, _) -> stub.
