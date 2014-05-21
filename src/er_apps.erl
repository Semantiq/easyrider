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

init(_Args) -> {ok, []}.

handle_call(apps, _From, State) -> {reply, {apps, State}, State};
handle_call({add_app, Application}, _From, State) ->
	{reply, ok, [#app{name = Application, stages = []} | State]};
handle_call({add_stage, AppName, StageName}, _From, State) ->
	{Result, NewState} = update_app(State, AppName,
		fun(App) -> {ok, App#app{stages = [#stage{name = StageName} | App#app.stages]}} end),
	{reply, Result, NewState};
handle_call({add_instance, AppName, StageName, Instance}, _From, State) ->
	{Result, NewState} = update_app(State, AppName, fun(App) ->
		update_stage(App, StageName, fun(Stage) ->
			{ok, Stage#stage{instances = [Instance | Stage#stage.instances]}}
		end)
	end),
	{reply, Result, NewState}.

update_app(State, AppName, Modification) ->
	case lists:keysearch(AppName, 2, State) of
		{value, App} ->
			case Modification(App) of
				{ok, NewApp} -> 
					NewState = lists:keyreplace(AppName, 2, State, NewApp),
					{ok, NewState};
				{Other, _} ->
					{Other, State}
			end;
		_ -> {unknown_app, State}
	end.
update_stage(App, StageName, Modification) ->
	case lists:keysearch(StageName, 2, App#app.stages) of
		{value, Stage} ->
			case Modification(Stage) of
				{ok, NewStage} ->
					NewApp = App#app{stages = lists:keyreplace(StageName, 2, App#app.stages, NewStage)},
					{ok, NewApp};
				{Other, _} ->
					{Other, App}
			end;
		_ -> {unknown_stage, App}
	end.	

%% Other gen_server callbacks

terminate(shutdown, _State) -> ok.
handle_cast(_Reg, State) -> {noreply, State}.
handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.
code_change(_OldVersion, State, _) -> {ok, State}.
