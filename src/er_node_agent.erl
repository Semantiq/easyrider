-module(er_node_agent).
-behaviour(gen_server).
-export([start_link/0, all_deployed_instances/0, deployed_instances/0, deploy_instance/4]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(deployed_instance, {id, agent, version, configuration}).

%% Interface

% @private
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
deployed_instances() -> gen_server:call(?MODULE, {deployed_instances}).
all_deployed_instances() -> gen_server:multi_call(?MODULE, {deployed_instances}).
deploy_instance(Node, Id, Version, Configuration) -> gen_server:call({?MODULE, Node}, {deploy_instance, Id, Version, Configuration}).

%% gen_server

init(_Args) -> {ok, []}.

handle_call({deployed_instances}, _From, State) -> {reply, {deployed_instances, State}, State};
handle_call({deploy_instance, Id, Version, Configuration}, _From, State) ->
	case orddict:find(Id, State) of
		{ok, DeployedInstance} ->
			er_instance_agent:destroy(DeployedInstance#deployed_instance.agent),
			{reply, {instance_updated}, new_instance(Id, Version, Configuration, State)};
		error ->
			{reply, {instance_deployed}, new_instance(Id, Version, Configuration, State)}
	end.

handle_cast(_Message, State) -> {noreply, State}.

%% helpers

new_instance(Id, Version, Configuration, State) ->
	Agent = er_instance_agent:start_link(Id, Version, Configuration),
	NewInstance = #deployed_instance{id = Id, agent = Agent, version = Version, configuration = Configuration},
	orddict:store(Id, NewInstance, State).

%% other gen_server

terminate(_, _) -> stub.
code_change(_, _, _) -> stub.
handle_info(_, _) -> stub.
