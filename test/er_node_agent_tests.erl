-module(er_node_agent_tests).
-compile(export_all).

init_test_() -> 
	fun() ->
		{ok, State} = er_node_agent:init(undefined),
		ok
	end.