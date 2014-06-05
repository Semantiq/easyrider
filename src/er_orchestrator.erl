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
	er_release_manager:subscribe_recommended_versions(self()),
	{ok, {}}.

handle_call(_Message, _From, State) -> {reply, ok, State}.

handle_cast({recommended_version, {{AppName, StageName}, Version, immediate}}, State) ->
	io:format("Performing recommened deployment: ~p v ~p to ~p~n", [AppName, Version, StageName]),
	{apps, Apps} = er_apps:apps(),
	Instances = get_instances(Apps, AppName, StageName),
	[ er_node_agent:deploy_instance(Node, Id, Version, Props) || #instance{id = Id, node = Node, properties = Props} <- Instances ],
	{noreply, State}.

%% helpers

get_instances(Apps, AppName, StageName) ->
	case lists:keysearch(AppName, 2, Apps) of
		% {value, App} ->
		% 	Stages = App#app.stages,
		% 	case lists:keysearch(StageName, 2, Stages) of
		% 		{value, Stage} ->
		% 			Stage#stage.instances;
		% 		false ->
		% 			io:format("Stage is not defined~n", []),
		% 			[]
		% 	end;
		false ->
			io:format("Application is not defined~n", []),
			[]
	end.
%% other gen_server

terminate(_, _) -> stub.
code_change(_, _, _) -> stub.
handle_info(_, _) -> stub.
