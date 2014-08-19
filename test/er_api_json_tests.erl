-module(er_api_json_tests).
-compile(export_all).
-define(TIMEOUT, 2000).

login_test_() -> { "Login test", easyrider_tests:in_clean_run(
	fun() ->
		{ok, Session} = er_api_json:new("{\"type\":\"login\", \"username\": \"test\", \"password\": \"test\"}"),
		er_api_json:tell(Session, "{\"type\":\"subscribe\", \"eventtypes\": [\"instance_events\", \"apps\"]}"),
		er_api_json:tell(Session, "{\"type\":\"setapp\", \"app\":{\"type\":\"app\", \"name\":\"my-app\", \"configuration\":{\"type\":\"configuration\", \"properties\": null}}}"),
		er_api_json:tell(Session, "{\"type\":\"setstage\", \"stage\":{\"type\":\"stage\", \"app\":\"my-app\", \"stage\":\"dev\"}}"),
		{struct, [
			{"type", "welcome"},
			{"role", "admin"}
		]} = expect_json_event(),
		{struct,[
			{"type", "snapshot"},
			{"eventtype", "instance_events"},
			{"data",[]}
		]} = expect_json_event(),
		{struct,[
			{"type", "snapshot"},
			{"eventtype", "apps"},
			{"data", _}
		]} = expect_json_event(),
		{struct,[
			{"type", "appupdated"},
			{"event", _},
			{"data", _}
		]} = expect_json_event(),
		er_api_json:tell(Session, "{\"type\":\"setinstance\", \"instance\":{\"type\":\"instance\", \"app\":\"my-app\", \"stage\":\"dev\", \"id\":\"app-0\", \"nodeid\":\"test0\"}}"),
		{struct,[
			{"type","instanceevent"},
			{"event", {struct,[
				{"type","event"},
				{"timestamp",_},
				{"eventtype","created"},
				{"key",{array,["app-0"]}}
			]}},
			{"versionnumber","no_version"}
		]} = expect_json_event()
	end)}.

expect_json_event() ->
	receive
		{_, Message} ->
			{ok, Struct} = json2:decode_string(lists:flatten(Message)),
			Struct
	after
		?TIMEOUT -> timeout
	end.
