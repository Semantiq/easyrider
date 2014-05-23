-module(easyrider).
-behaviour(application).
-export([start/2, stop/1]).

start(_Mode, _Args) ->
	Sup = er_supervisor:start_link(),
	yaws_config(),
	Sup.

stop(_State) -> ok.

yaws_config() ->
	Port = case application:get_env(easyrider, webconsole_port) of
		{ok, FromConfig} -> FromConfig;
		_ -> 8000
	end,
	Id = "embedded",
	GconfList = [{id, Id}],
	DocRoot = "web",
	SConfList = [
		{port, Port},
		{listen, {0, 0, 0, 0}},
		{docroot, DocRoot},
		{appmods, [
			{"/api", er_webconsole}
		]}
	],
	{ok, SCList, GC, Children} = yaws_api:embedded_start_conf(DocRoot, SConfList, GconfList, Id),
	[supervisor:start_child(er_supervisor, Child) || Child <- Children],
	io:format("yaws:~n~p~n~p~n", [GC, SCList]),
	yaws_api:setconf(GC, SCList).
