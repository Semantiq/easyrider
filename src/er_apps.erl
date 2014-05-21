-module(er_apps).
-behaviour(gen_server).
-include("er_apps.hrl").
-export([start_link/0, apps/0, add_app/1, add_stage/2, add_instance/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

%% Interface

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

apps() -> gen_server:call({global, ?MODULE}, apps).
add_app(Application) -> gen_server:call({global, ?MODULE}, {add_app, Application}).
add_stage(Application, Stage) -> gen_server:call({global, ?MODULE}, {add_stage, Application, Stage}).
add_instance(Application, Stage, Instance) -> gen_server:call({global, ?MODULE}, {add_instance, Application, Stage, Instance}).

%% gen_server

init(_Args) -> {ok, #state{}}.

handle_call(apps, _From, State) ->
	{reply, {apps, [#app{name = AppName, properties = Properties, stages = find_stages(State, AppName)} ||
		{AppName, Properties} <- State#state.apps]}, State};
handle_call({add_app, Application}, _From, State) ->
	{reply, ok, State#state{apps = orddict:store(Application, [], State#state.apps)}};
handle_call({add_stage, AppName, StageName}, _From, State) ->
	{reply, ok, State#state{stages = orddict:store({AppName, StageName}, [], State#state.stages)}};
handle_call({add_instance, AppName, StageName, Instance}, _From, State) ->
	{reply, ok, State#state{instances = orddict:store({AppName, StageName, Instance#instance.id}, Instance, State#state.instances)}}.

find_stages(#state{stages = Stages} = State, AppName) ->
	AppStages = lists:filter(fun({{CurrentAppName, _}, _}) -> CurrentAppName == AppName end, Stages),
	[#stage{name = StageName, properties = Properties, instances = find_instances(State, AppName, StageName)} ||
		{{_, StageName}, Properties} <- AppStages].

find_instances(#state{instances = Instances}, AppName, StageName) ->
	lists:flatmap(fun({Key, Instance}) ->
		case Key of
			{AppName, StageName, _} -> [Instance];
			_ -> []
		end
	end, Instances).

%% Other gen_server callbacks

terminate(shutdown, _State) -> ok.
handle_cast(_Reg, State) -> {noreply, State}.
handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.
code_change(_OldVersion, State, _) -> {ok, State}.
