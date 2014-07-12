-module(er_api_json_tests).
-compile(export_all).
-define(TIMEOUT, 2000).

login_test_() -> { "Login test", easyrider_tests:in_clean_run(
	fun() ->
		{ok, Session} = er_api_json:new("{\"type\":\"login\", \"username\": \"test\", \"password\": \"test\"}"),
		er_api_json:tell(Session, "{\"type\":\"subscribe\", \"eventtypes\": [\"instance_events\"]}"),
		er_api_json:tell(Session, "{\"type\":\"setapp\", \"app\":{\"type\":\"app\", \"name\":\"my-app\", \"configuration\":{\"type\":\"configuration\", \"properties\": null}}}"),
		{struct, [
			{"type", "welcome"},
			{"role", "admin"}
		]} = expect_json_event()
		%% "" = expect_json_event()
	end)}.

expect_json_event() ->
	receive
		{_, Message} ->
			{ok, Struct} = json2:decode_string(lists:flatten(Message)),
			Struct
	after
		?TIMEOUT -> timeout
	end.
