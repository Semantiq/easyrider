-module(er_apps).
-behaviour(gen_server).
-include("er_apps.hrl").
-export([start_link/0, apps/0, subscribe_apps/1, add_app/1, add_stage/2, add_instance/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {apps = [], stages = [], instances = [], subscriptions = []}).

%% Interface

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

apps() -> gen_server:call({global, ?MODULE}, apps).
subscribe_apps(Pid) -> gen_server:cast({global, ?MODULE}, {subscribe_apps, Pid}).
add_app(Application) -> gen_server:call({global, ?MODULE}, {add_app, Application}).
add_stage(Application, Stage) -> gen_server:call({global, ?MODULE}, {add_stage, Application, Stage}).
add_instance(Application, Stage, Instance) -> gen_server:call({global, ?MODULE}, {add_instance, Application, Stage, Instance}).

%% gen_server

init(_Args) -> {ok, load_state()}.

handle_call(apps, _From, State) ->
	{reply, {apps, get_apps(State)}, State};
handle_call({add_app, Application}, _From, State) ->
	NewState = State#state{apps = orddict:store(Application, [], State#state.apps)},
	notify_subscribers(NewState),
	{reply, ok, NewState};
handle_call({add_stage, AppName, StageName}, _From, State) ->
	NewState = State#state{stages = orddict:store({AppName, StageName}, [], State#state.stages)},
	notify_subscribers(NewState),
	{reply, ok, NewState};
handle_call({add_instance, AppName, StageName, Instance}, _From, State) ->
	NewState = State#state{instances = orddict:store({AppName, StageName, Instance#instance.id}, Instance, State#state.instances)},
	notify_subscribers(NewState),
	{reply, ok, NewState}.

handle_cast({subscribe_apps, Pid}, State) ->
	erlang:monitor(process, Pid),
	gen_server:cast(Pid, {apps, get_apps(State)}),
	{noreply, State#state{subscriptions = [Pid | State#state.subscriptions]}}.

% helpers

get_apps(State) ->
	[#app{name = AppName, properties = Properties, stages = find_stages(State, AppName)} ||
	{AppName, Properties} <- State#state.apps].

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

notify_subscribers(State) ->
	io:format("Notifying: ~p~n", [State#state.subscriptions]),
	Apps = get_apps(State),
	store_state(State#state{subscriptions = []}),
	[ gen_server:cast(Pid, {apps, Apps}) || Pid <- State#state.subscriptions ].

store_state(Data) ->
	% TODO: dedicated async process for storing stuff
	file:write_file("data/apps.config",io_lib:fwrite("~p.\n",[Data])).
load_state() ->
	case file:consult("data/apps.config") of
		{ok, [State]} ->
			State#state{subscriptions = []};
		_ -> #state{}
	end.

%% Other gen_server callbacks

terminate(shutdown, _State) -> ok.
handle_info({'DOWN', _, process, Pid, _}, State) ->
	io:format("Removing ~p from subscriptions~n", [Pid]),
	{noreply, State#state{subscriptions = lists:delete(Pid, State#state.subscriptions)}};
handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.
code_change(_OldVersion, State, _) -> {ok, State}.
