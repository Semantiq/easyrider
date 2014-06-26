-module(easyrider_tests).
-compile(export_all).

-define(TIMEOUT, 2000).

start_and_stop_test_() -> {"Startup test", in_clean_run(fun() -> ok end)}.

configure_app_test_() -> {"Upload, deploy and start test app", in_clean_run(
	fun() ->
		er_event_bus:subscribe(self(), [instance_events]),
		er_apps:set_app({app, "app", []}),
		er_apps:set_stage({stage, "app", "dev", []}),
		er_apps:set_instance({instance, "app", "dev", "app-0", "test0", [
			{property, "port", "8080"}
		]}),
		er_repository:upload_version_file("app", "1.0", "../test_app/test_app.zip"),
		%er_apps:tell_instance("app-0", {deploy, "1.0"}),
		{deploying, _} = expect_event(instance_events, "app-0"),
		{running, _} = expect_event(instance_events, "app-0")
	end)}.

expect_event(Type, Key) ->
	receive
		{_, {event, Type, Key, Value}} -> Value
	after
		?TIMEOUT -> fail
	end.

in_clean_run(Scenario) ->
	fun() ->
		os:cmd("rm -r data_test"),
		configure(),
		application:start(easyrider),
		Scenario(),
		application:stop(easyrider)
	end.		

configure() ->
	Config = [
		{webconsole_port, 8001},
		{webconsole_docroot, "../web"},
		{data_directory, "data_test/"},
		{users, [
			{"test", <<9,143,107,205,70,33,211,115,202,222,78,131,38,39,180,246>>, admin}
		]},
		{node_id, "test0"},
		{run_event_bus, true},
		{run_apps, true},
		{run_repository, true},
		{run_repository_storage, true},
		{run_release_manager, true},
		{run_orchestrator, true},
		{run_user_manager, true},
		{run_node_manager, true}
	],
	[ application:set_env(easyrider, Key, Value) || {Key, Value} <- Config ].
