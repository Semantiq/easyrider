-module(er_apps).
-behaviour(gen_server).
-include("easyrider_pb.hrl").
-export([start_link/0, set_app/1, set_stage/1, set_instance/1, effective_configuration/3, tell_instance/2, get_instance/1, remove_stage/2, remove_app/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {apps = [], stages = [], instances = []}).

%% Interface

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

set_app(Application) -> gen_server:call({global, ?MODULE}, {set_app, Application}).
remove_app(AppName) -> gen_server:call({global, ?MODULE}, {remove_app, AppName}).
set_stage(Stage) -> gen_server:call({global, ?MODULE}, {set_stage, Stage}).
remove_stage(AppName, StageName) -> gen_server:call({global, ?MODULE}, {remove_stage, AppName, StageName}).
set_instance(Instance) -> gen_server:call({global, ?MODULE}, {set_instance, Instance}).
effective_configuration(AppName, StageName, Id) -> gen_server:call({global, ?MODULE}, {effective_configuration, AppName, StageName, Id}).
tell_instance(Id, Message) -> gen_server:cast({global, ?MODULE}, {tell_instance, Id, Message}).
get_instance(Id) -> gen_server:call({global, ?MODULE}, {get_instance, Id}).

%% gen_server

init(_Args) ->
	{snapshot, apps, Apps} = er_event_bus:get_snapshot(apps),
	{snapshot, stages, Stages} = er_event_bus:get_snapshot(stages),
	{snapshot, instances, Instances} = er_event_bus:get_snapshot(instances),
	er_event_bus:subscribe(self(), [instance_events]),
	{ok, #state{apps = Apps, stages = Stages, instances = Instances}}.

handle_call({set_app, #app{name = AppName} = Application}, _From, State) ->
	NewApps = orddict:store(AppName, Application, State#state.apps),
	er_event_bus:publish({apps, AppName, Application}),
	{reply, ok, State#state{apps = NewApps}};
handle_call({remove_app, AppName}, _From, State) ->
	StageNames = [ StageName || {{ThisAppName, StageName}, _Stage} <- State#state.stages, ThisAppName == AppName ],
	case StageNames of
		[] ->
			NewApps = orddict:erase(AppName, State#state.apps),
			er_event_bus:publish({apps, AppName, remove}),
			{reply, ok, State#state{apps = NewApps}};
		_ ->
			{reply, {pending_stages, StageNames}, State}
	end;
handle_call({set_stage, #stage{app = AppName, stage = StageName} = Stage}, _From, State) ->
	case orddict:is_key(AppName, State#state.apps) of
		true ->
			NewStages = orddict:store({AppName, StageName}, Stage, State#state.stages),
			er_event_bus:publish({stages, {AppName, StageName}, Stage}),
			{reply, ok, State#state{stages = NewStages}};
		false ->
			{reply, no_app, State}
	end;
handle_call({remove_stage, AppName, StageName}, _From, State) ->
	InstanceIds = [ Id || {{ThisAppName, ThisStageName, Id}, _Instance} <- State#state.instances, ThisAppName == AppName, ThisStageName == StageName ],
	case InstanceIds of
		[] ->
			NewStages = orddict:erase({AppName, StageName}, State#state.stages),
			er_event_bus:publish({stages, {AppName, StageName}, remove}),
			{reply, ok, State#state{stages = NewStages}};
		_ ->
			{reply, {pending_instances, InstanceIds}, State}
	end;
handle_call({set_instance, #instance{app = AppName, stage = StageName, id = Id} = Instance}, _From, State) ->
	case orddict:is_key({AppName, StageName}, State#state.stages) of
		true ->
			NewInstances = orddict:store({AppName, StageName, Id}, Instance, State#state.instances),
			er_event_bus:publish({instances, {AppName, StageName, Id}, Instance}),
			{reply, ok, State#state{instances = NewInstances}};
		false ->
			{reply, no_app_stage, State}
	end;
handle_call({effective_configuration, AppName, StageName, Id}, _From, #state{apps = Apps, stages = Stages, instances = Instances} = State) ->
	{ok, #app{configuration = AppConfiguration}} = orddict:find(AppName, Apps),
	{ok, #stage{configuration = StageConfiguration}} = orddict:find({AppName, StageName}, Stages),
	{ok, #instance{configuration = InstanceConfiguration}} = orddict:find({AppName, StageName, Id}, Instances),
	Properties = flatten_properties(AppConfiguration#configuration.properties ++
			StageConfiguration#configuration.properties ++
			InstanceConfiguration#configuration.properties),
	WrapperProperties = flatten_properties(AppConfiguration#configuration.wrapperproperties ++
			StageConfiguration#configuration.wrapperproperties ++
			InstanceConfiguration#configuration.wrapperproperties),
	{reply, {ok, #configuration{properties = Properties, wrapperproperties = WrapperProperties}}, State};
handle_call({get_instance, Id}, _From, State) ->
	Response = case lists:filter(fun({_, #instance{id = ThisId}}) -> ThisId == Id end, State#state.instances) of
		[{_, Instance}] -> {instance, Instance};
		_ -> error
	end,
	{reply, Response, State}.

handle_cast({tell_instance, Id, Message}, State) ->
	[{_, #instance{nodeid = NodeId}}] = lists:filter(fun({_, #instance{id = ThisId}}) -> ThisId == Id end, State#state.instances),
	er_node_manager:tell_node(NodeId, {tell_instance, Id, Message}),
	{noreply, State};
handle_cast({event, instance_events, Id, remove}, State) ->
	Partition = lists:partition(fun({_, #instance{id = ThisId}}) -> ThisId == Id end, State#state.instances),
	case Partition of
		{[{InstanceKey, _}], NewInstances} ->
			er_event_bus:publish({instances, InstanceKey, remove}),
			{noreply, State#state{instances = NewInstances}};
		_ -> {noreply, State}
	end;
handle_cast({event, instance_events, _, _}, State) -> {noreply, State};
handle_cast({snapshot, instance_events, _Data}, State) -> {noreply, State}.

%% helpers

flatten_properties(Properties) -> flatten_properties([], Properties).
flatten_properties(Dict, []) -> [ {Type, Name, Value} || {{Type, Name}, Value} <- Dict ];
flatten_properties(Dict, [{Type, Name, Value} | Properties]) -> flatten_properties(orddict:store({Type, Name}, Value, Dict), Properties).

%% Other gen_server callbacks

terminate(_Reason, _State) -> ok.
handle_info(_, _) -> stub.
code_change(_, _, _) -> stub.
