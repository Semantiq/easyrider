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
			{"/api", er_webconsole},
			{"/repo", er_repository_web}
		]}
	],
	{ok, SCList, GC, Children} = yaws_api:embedded_start_conf(DocRoot, SConfList, GconfList, Id),
	[supervisor:start_child(er_supervisor, Child) || Child <- Children],
	yaws_api:setconf(GC, SCList).
