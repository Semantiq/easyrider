%% @doc Maintains the state of nodes. Allows to sent messages to nodes and instances by logical name.
-module(er_node_manager).
-include("er_apps.hrl").
-behaviour(gen_server).
-export([start_link/0, node_up/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {instances = [], nodes = []}).

%% Interface

% @private
start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
node_up(NodeId, Node) -> gen_server:cast({global, ?MODULE}, {node_up, NodeId, Node}).

%% gen_server

init(_Args) ->
	{snapshot, nodes, Nodes} = er_event_bus:get_snapshot(nodes),
	{LiveNodes, Zombies} = lists:partition(fun live_node/1, Nodes),
	[er_event_bus:publish({nodes, NodeId, remove}) || {NodeId, _Node} <- Zombies],
	er_event_bus:subscribe(self(), [instances]),
	{ok, #state{nodes = LiveNodes}}.

handle_info({nodedown, Node}, State) ->
	io:format("Node leaving: ~p~n", [Node]),
	{[{NodeId, _}], RemindingNodes} = lists:partition(find_by_node(Node), State#state.nodes),
	er_event_bus:publish({nodes, NodeId, remove}),
	{noreply, State#state{nodes = RemindingNodes}}.

handle_cast({node_up, NodeId, Node}, State) ->
	io:format("Node joining: ~p (as ~p~n)", [Node, NodeId]),
	erlang:monitor_node(Node, true),
	er_event_bus:publish({nodes, NodeId, Node}),
	er_node_agent:on_join(Node),
	Nodes = orddict:store(NodeId, Node, State#state.nodes),
	{noreply, State#state{nodes = Nodes}};
handle_cast({snapshot, instances, _Data}, State) -> {noreply, State};
handle_cast({event, instances, _Key, _Value}, State) -> {noreply, State}.

%% helpers

find_by_node(Node) -> fun({_, ANode}) -> ANode == Node end.
live_node({_NodeId, Node}) -> net_adm:ping(Node) == pong.

%% other gen_server

handle_call(_, _, _) -> stub.
terminate(_, _) -> stub.
code_change(_, _, _) -> stub.
