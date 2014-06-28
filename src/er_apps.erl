-module(er_apps).
-behaviour(gen_server).
-include("er_apps.hrl").
-export([start_link/0, set_app/1, set_stage/1, set_instance/1, effective_configuration/3, tell_instance/2, get_instance/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {apps = [], stages = [], instances = []}).

%% Interface

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

set_app(Application) -> gen_server:call({global, ?MODULE}, {set_app, Application}).
set_stage(Stage) -> gen_server:call({global, ?MODULE}, {set_stage, Stage}).
set_instance(Instance) -> gen_server:call({global, ?MODULE}, {set_instance, Instance}).
effective_configuration(AppName, StageName, Id) -> gen_server:call({global, ?MODULE}, {effective_configuration, AppName, StageName, Id}).
tell_instance(Id, Message) -> gen_server:cast({global, ?MODULE}, {tell_instance, Id, Message}).
get_instance(Id) -> gen_server:call({global, ?MODULE}, {get_instance, Id}).

%% gen_server

init(_Args) ->
	{snapshot, apps, Apps} = er_event_bus:get_snapshot(apps),
	{snapshot, stages, Stages} = er_event_bus:get_snapshot(stages),
	{snapshot, instances, Instances} = er_event_bus:get_snapshot(instances),
	{ok, #state{apps = Apps, stages = Stages, instances = Instances}}.

handle_call({set_app, #app{app_name = AppName} = Application}, _From, State) ->
	NewApps = orddict:store(AppName, Application, State#state.apps),
	er_event_bus:publish({apps, AppName, Application}),
	{reply, ok, State#state{apps = NewApps}};
handle_call({set_stage, #stage{app_name = AppName, stage_name = StageName} = Stage}, _From, State) ->
	case orddict:is_key(AppName, State#state.apps) of
		true ->
			NewStages = orddict:store({AppName, StageName}, Stage, State#state.stages),
			er_event_bus:publish({stages, {AppName, StageName}, Stage}),
			{reply, ok, State#state{stages = NewStages}};
		false ->
			{reply, no_app, State}
	end;
handle_call({set_instance, #instance{app_name = AppName, stage_name = StageName, id = Id} = Instance}, _From, State) ->
	case orddict:is_key({AppName, StageName}, State#state.stages) of
		true ->
			NewInstances = orddict:store({AppName, StageName, Id}, Instance, State#state.instances),
			er_event_bus:publish({instances, {AppName, StageName, Id}, Instance}),
			{reply, ok, State#state{instances = NewInstances}};
		false ->
			{reply, no_app_stage, State}
	end;
handle_call({effective_configuration, AppName, StageName, Id}, _From, #state{apps = Apps, stages = Stages, instances = Instances} = State) ->
	{ok, #app{properties = AppProperties}} = orddict:find(AppName, Apps),
	{ok, #stage{properties = StageProperties}} = orddict:find({AppName, StageName}, Stages),
	{ok, #instance{properties = InstanceProperties}} = orddict:find({AppName, StageName, Id}, Instances),
	{reply, flatten_properties(AppProperties ++ StageProperties ++ InstanceProperties), State};
handle_call({get_instance, Id}, _From, State) ->
	Response = case lists:filter(fun({_, #instance{id = ThisId}}) -> ThisId == Id end, State#state.instances) of
		[{_, Instance}] -> {instance, Instance};
		_ -> error
	end,
	{reply, Response, State}.

handle_cast({tell_instance, Id, Message}, State) ->
	[{_, #instance{node = NodeId}}] = lists:filter(fun({_, #instance{id = ThisId}}) -> ThisId == Id end, State#state.instances),
	er_node_manager:tell_node(NodeId, {tell_instance, Id, Message}),
	{noreply, State}.

%% helpers

flatten_properties(Properties) -> flatten_properties([], Properties).
flatten_properties(Dict, []) -> [ {Type, Name, Value} || {{Type, Name}, Value} <- Dict ];
flatten_properties(Dict, [{Type, Name, Value} | Properties]) -> flatten_properties(orddict:store({Type, Name}, Value, Dict), Properties).

%% Other gen_server callbacks

terminate(_Reason, _State) -> ok.
handle_info(_, _) -> stub.
code_change(_, _, _) -> stub.
