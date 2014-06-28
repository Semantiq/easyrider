-module(er_node_agent).
-include("er_apps.hrl").
-behaviour(gen_server).
-export([start_link/0, all_deployed_instances/0, deployed_instances/0, on_join/1, tell_instance/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {instances, joined = false}).
-record(deployed_instance, {id, agent}).

-define(JOIN_ATTEMPTS_INTERVAL, 1000).

%% Interface

% @private
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
deployed_instances() -> gen_server:call(?MODULE, {deployed_instances}).
all_deployed_instances() -> gen_server:multi_call(?MODULE, {deployed_instances}).
on_join(Node) -> gen_server:cast({?MODULE, Node}, on_join).
tell_instance(Node, Id, Message) -> gen_server:cast({?MODULE, Node}, {tell_instance, Id, Message}).

%% gen_server

init(_Args) ->
	er_event_bus:subscribe(self(), [instances]),
	join_attempt(),
	{ok, #state{instances = load_state()}, ?JOIN_ATTEMPTS_INTERVAL}.

handle_call({deployed_instances}, _From, State) -> {reply, {deployed_instances, State#state.instances}, State}.

handle_cast(on_join, State) ->
	error_logger:info_msg("Joined cluster~n", []),
	{noreply, State#state{joined = true}};
handle_cast({tell_instance, Id, Message}, State) ->
	%% TODO: handle incorrect id
	case orddict:find(Id, State#state.instances) of
		{ok, #deployed_instance{agent = Agent}} -> gen_server:cast(Agent, Message);
		_ -> error_logger:error_msg("Message to istance ~p dropped: ~p~n", [Id, Message])
	end,
	{noreply, State};
handle_cast({event, instances, _, Instance}, State) ->
	NewState = process_instance(Instance, State),
	{noreply, NewState};
handle_cast({snapshot, instances, Data}, State) ->
	Instances = [ Instance || {_, Instance} <- Data ],
	NewState = process_instances(Instances, State),
	{noreply, NewState}.

handle_info(timeout, #state{joined = true} = State) ->
	{noreply, State};
handle_info(timeout, #state{joined = false} = State) ->
	join_attempt(),
	{noreply, State, ?JOIN_ATTEMPTS_INTERVAL};
handle_info({'DOWN', _, process, Pid, _}, State) ->
	error_logger:error_msg("Instance agent down ~p~n", [Pid]),
	NewInstances = lists:filter(fun({_Id, #deployed_instance{agent = Agent}}) -> Agent /= Pid end, State#state.instances),
	{noreply, State#state{instances = NewInstances}}.

%% helpers

process_instances([], State) -> State;
process_instances([Instance | Rest], State) -> process_instances(Rest, process_instance(Instance, State)).

process_instance(Instance, State) ->
	NodeId = node_id(),
	case Instance of
		#instance{id = Id, node = NodeId} ->
			case orddict:find(Id, State#state.instances) of
				{ok, #deployed_instance{agent = _Agent}} ->
					error_logger:info_msg("TODO: Instance config change: ~p~n", [Instance]),
					State;
				_ -> new_instance(Id, State)
			end;
		#instance{id = _Id} ->
			%% TODO: find out if we owned that instance, remove if we did
			State
	end.

node_id() -> {ok, NodeId} = application:get_env(easyrider, node_id), NodeId.

join_attempt() ->
	NodeId = node_id(),
	io:format("Joining as ~p (~p)~n", [node(), NodeId]),
	er_node_manager:node_up(NodeId, node()).

start_new_instance(Id) ->
	{ok, Agent} = er_instance_agent:start_link(Id),
	erlang:monitor(process, Agent),
	#deployed_instance{id = Id, agent = Agent}.

new_instance(Id, State) ->
	DeployedInstance = start_new_instance(Id),
	NewInstances = orddict:store(Id, DeployedInstance, State#state.instances),
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
			[ {Id, start_new_instance(Id)} || {Id, _} <- DeployedInstances];
		_ -> []
	end.

%% other gen_server

terminate(_, _) -> stub.
code_change(_, _, _) -> stub.
