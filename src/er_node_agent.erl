-module(er_node_agent).
-behaviour(gen_server).
-export([start_link/0, all_deployed_instances/0, deployed_instances/0, deploy_instance/4, on_join/1, tell_instance/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {instances, joined = false}).
-record(deployed_instance, {id, agent, version, configuration}).

%% Interface

% @private
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
deployed_instances() -> gen_server:call(?MODULE, {deployed_instances}).
all_deployed_instances() -> gen_server:multi_call(?MODULE, {deployed_instances}).
deploy_instance(NodeId, Id, Version, Configuration) -> er_node_manager:tell_node(NodeId, {deploy_instance, Id, Version, Configuration}).
on_join(Node) -> gen_server:cast({?MODULE, Node}, on_join).
tell_instance(Node, Id, Message) -> gen_server:cast({?MODULE, Node}, {tell_instance, Id, Message}).

%% gen_server

init(_Args) ->
	{ok, #state{instances = load_state()}, 1000}.

handle_call({deployed_instances}, _From, State) -> {reply, {deployed_instances, State#state.instances}, State}.

handle_cast({deploy_instance, Id, Version, Configuration}, State) ->
	case orddict:find(Id, State#state.instances) of
		{ok, DeployedInstance} ->
			er_instance_agent:destroy(DeployedInstance#deployed_instance.agent),
			{noreply, new_instance(Id, Version, Configuration, State)};
		error ->
			{noreply, new_instance(Id, Version, Configuration, State)}
	end;
handle_cast(on_join, State) ->
	io:format("Joined cluster~n", []),
	{noreply, State#state{joined = true}};
handle_cast({tell_instance, Id, Message}, State) ->
	%% TODO: handle incorrect id
	{ok, #deployed_instance{agent = Agent}} = orddict:find(Id, State#state.instances),
	gen_server:cast(Agent, Message),
	{noreply, State}.

handle_info(timeout, #state{joined = true} = State) ->
	{noreply, State};
handle_info(timeout, #state{joined = false} = State) ->
	{ok, NodeId} = application:get_env(easyrider, node_id),
	io:format("Joining as ~p (~p)~n", [node(), NodeId]),
	er_node_manager:node_up(NodeId, node()),
	{noreply, State, 2000};
handle_info({'DOWN', _, process, Pid, _}, State) ->
	io:format("Instance agent down ~p~n", [Pid]),
	NewInstances = lists:filter(fun({_Id, #deployed_instance{agent = Agent}}) -> Agent /= Pid end, State#state.instances),
	{noreply, State#state{instances = NewInstances}}.

%% helpers

start_new_instance(Id, Version, Configuration) ->
	{ok, Agent} = er_instance_agent:start_link(Id, Version, Configuration),
	erlang:monitor(process, Agent),
	#deployed_instance{id = Id, agent = Agent, version = Version, configuration = Configuration}.

new_instance(Id, Version, Configuration, State) ->
	NewInstance = start_new_instance(Id, Version, Configuration),
	NewInstances = orddict:store(Id, NewInstance, State#state.instances),
	NewState = State#state{instances = NewInstances},
	store_state(NewInstances),
	NewState.

node_config() -> er_configuration:data_directory() ++ "/node.config".
store_state(DeployedInstances) ->
	% TODO: dedicated async process for storing stuff
	file:write_file(node_config(),io_lib:fwrite("~p.\n", [[{Id, DeployedInstance#deployed_instance{agent = undefined}} || {Id, DeployedInstance} <- DeployedInstances]])).
load_state() ->
	case file:consult(node_config()) of
		{ok, [DeployedInstances]} ->
			[ {Id, start_new_instance(Id, Version, Configuration)} || {Id, #deployed_instance{version = Version, configuration = Configuration}} <- DeployedInstances];
		_ -> []
	end.

%% other gen_server

terminate(_, _) -> stub.
code_change(_, _, _) -> stub.
