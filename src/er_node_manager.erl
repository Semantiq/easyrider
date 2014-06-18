-module(er_node_manager).
-include("er_apps.hrl").
-export([start_instance/1, stop_instance/1]).

%% TODO: this needs to be a process

start_instance(InstanceId) ->
	{snapshot, Instances} = er_event_bus:get_snapshot(instances),
	{ok, #instance{node = Node}} = orddict:find(InstanceId, Instances),
	er_node_agent:start_instance(Node, InstanceId).

stop_instance(InstanceId) ->
	{snapshot, Instances} = er_event_bus:get_snapshot(instances),
	{ok, #instance{node = Node}} = orddict:find(InstanceId, Instances),
	er_node_agent:stop_instance(Node, InstanceId).
