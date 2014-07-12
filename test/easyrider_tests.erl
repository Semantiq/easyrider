-module(easyrider_tests).
-include("easyrider_pb.hrl").
-compile(export_all).

-define(TIMEOUT, 2000).

start_and_stop_test_() -> {"Startup test", in_clean_run(fun() -> ok end)}.

reject_ivalid_password_test_() -> {"Invalid password", in_clean_run(
	fun() ->
		#rejected{message = "bad_username_or_password"} = er_api:new(#login{username = "test", password = "invalid password"})
	end)}.

configure_app_test_() -> {"Upload, deploy and start test app", in_clean_run(
	fun() ->
		{ok, Session} = er_api:new(#login{username = "test", password = "test"}),
		er_api:tell(Session, #subscribe{eventtypes = ["instance_events"]}),
		er_api:tell(Session, #setapp{app = #app{name = "app", configuration = #configuration{}}}),
		er_api:tell(Session, #setstage{stage = #stage{app = "app", stage = "dev", configuration = #configuration{}}}),
		er_api:tell(Session, #setinstance{
			instance = #instance{app = "app", stage = "dev", id = "app-0", nodeid = "test0", configuration = #configuration{
				properties = [
					{property, "port", "8080"}
				]
			}}
		}),
		er_repository:upload_version_file("app", "1.0", "../test_app/test_app.zip"),
		{"created", "no_version"} = expect_event(instanceevent, ["app-0"]),
		{"deploying", "1.0"} = expect_event(instanceevent, ["app-0"]),
		{"running", "1.0"} = expect_event(instanceevent, ["app-0"])
	end)}.

redeployment_test_() -> {"Redeploy to instance", in_clean_run(
	fun() ->
		{ok, Session} = er_api:new(#login{username = "test", password = "test"}),
		er_api:tell(Session, #subscribe{eventtypes = ["instance_events", "versions"]}),
		er_repository:upload_version_file("app", "1.0", "../test_app/test_app.zip"),
		er_repository:upload_version_file("app", "2.0", "../test_app/test_app.zip"),
		{"new_version", "1.0"} = expect_event(newversion, ["app"]),
		{"new_version", "2.0"} = expect_event(newversion, ["app"]),
		er_api:tell(Session, #setapp{app = #app{name = "app", configuration = #configuration{}}}),
		er_api:tell(Session, #setstage{stage = #stage{app = "app", stage = "dev", configuration = #configuration{}}}),
		er_api:tell(Session, #setinstance{
			instance = #instance{app = "app", stage = "dev", id = "app-0", nodeid = "test0", configuration = #configuration{
				properties = [
					{property, "port", "8080"}
				]
			}}
		}),
		{"created", "no_version"} = expect_event(instanceevent, ["app-0"]),
		er_api:tell(Session, #deployinstance{instanceid = "app-0", versionnumber = "1.0"}),
		{"deploying", "1.0"} = expect_event(instanceevent, ["app-0"]),
		{"running", "1.0"} = expect_event(instanceevent, ["app-0"]),
		er_api:tell(Session, #deployinstance{instanceid = "app-0", versionnumber = "2.0"}),
		{"stopping", "1.0"} = expect_event(instanceevent, ["app-0"]),
		{"stopped", "1.0"} = expect_event(instanceevent, ["app-0"]),
		{"deploying", "2.0"} = expect_event(instanceevent, ["app-0"]),
		{"running", "2.0"} = expect_event(instanceevent, ["app-0"])
	end)}.

app_state_instance_remove_test_() -> {"Remove app, stage, created instance", in_clean_run(
	fun() ->
		{ok, Session} = er_api:new(#login{username = "test", password = "test"}),
		er_api:tell(Session, #subscribe{eventtypes = ["instance_events", "instances", "apps"]}),
		er_api:tell(Session, #setapp{app = #app{name = "app", configuration = #configuration{}}}),
		{"apps", _} = expect_event(appupdated, ["app"]),
		er_api:tell(Session, #setstage{stage = #stage{app = "app", stage = "dev", configuration = #configuration{}}}),
		er_api:tell(Session, #setinstance{
			instance = #instance{app = "app", stage = "dev", id = "app-0", nodeid = "test0", configuration = #configuration{
				properties = [
					{property, "port", "8080"}
				]
			}}
		}),
		{"created", "no_version"} = expect_event(instanceevent, ["app-0"]),
		{"instances", _} = expect_event(instanceupdated, ["app", "dev", "app-0"]),
		er_api:tell(Session, #removeinstance{instanceid = "app-0"}),
		{"remove", _} = expect_event(instanceevent, ["app-0"]),
		{"remove", _} = expect_event(instanceupdated, ["app", "dev", "app-0"]),
		er_api:tell(Session, #removestage{app = "app", stage = "dev"}),
		er_api:tell(Session, #removeapp{app = "app"}),
		{"remove", _} = expect_event(appupdated, ["app"])
	end)}.

running_instance_remove_test_() -> {"Remove running instance", in_clean_run(
	fun() ->
		{ok, Session} = er_api:new(#login{username = "test", password = "test"}),
		er_api:tell(Session, #subscribe{eventtypes = ["instance_events", "instances"]}),
		er_api:tell(Session, #subscribe{eventtypes = ["instance_events", "instances", "apps"]}),
		er_api:tell(Session, #setapp{app = #app{name = "app", configuration = #configuration{}}}),
		er_api:tell(Session, #setstage{stage = #stage{app = "app", stage = "dev", configuration = #configuration{}}}),
		er_api:tell(Session, #setinstance{
			instance = #instance{app = "app", stage = "dev", id = "app-0", nodeid = "test0", configuration = #configuration{
				properties = [
					{property, "port", "8080"}
				]
			}}
		}),
		{"created", "no_version"} = expect_event(instanceevent, ["app-0"]),
		{"instances", _} = expect_event(instanceupdated, ["app", "dev", "app-0"]),
		er_repository:upload_version_file("app", "1.0", "../test_app/test_app.zip"),
		{"deploying", _} = expect_event(instanceevent, ["app-0"]),
		{"running", _} = expect_event(instanceevent, ["app-0"]),
		er_api:tell(Session, #removeinstance{instanceid = "app-0"}),
		{"stopping", _} = expect_event(instanceevent, ["app-0"]),
		{"stopped", _} = expect_event(instanceevent, ["app-0"]),
		{"remove", _} = expect_event(instanceevent, ["app-0"]),
		{"remove", _} = expect_event(instanceupdated, ["app", "dev", "app-0"])
	end)}.

expect_event_actual(Expectation, Actual) ->
			receive
				Message -> expect_event_actual(Expectation, Actual ++ [Message])
			after
				?TIMEOUT -> {message_didnt_arrive, Expectation, Actual}
			end.

expect_event(Type, Key) ->
	receive
		{_, {Type, #event{eventtype = Event, key = Key}, Data}} -> {Event, Data}
	after
		?TIMEOUT -> expect_event_actual({Type, Key}, [])
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
			{"test", <<159,134,208,129,136,76,125,101,154,47,234,160,197,90,208,21,163,191,79,27,43,11,130,44,209,93,108,21,176,240,10,8>>, admin}
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
