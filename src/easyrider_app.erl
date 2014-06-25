-module(easyrider_app).
-behaviour(application).
-export([start/2, stop/1]).

%% Application callbacks

start(_Mode, _Args) ->
	error_logger:logfile({open, "easyrider.log"}),
	{ok, Sup} = easyrider_sup:start_link(),
	yaws_config(),
	{ok, Sup}.

stop(_State) -> ok.

%% Helpers

yaws_config() ->
	Port = case application:get_env(easyrider, webconsole_port) of
		{ok, FromConfig} -> FromConfig;
		_ -> 8000
	end,
	Id = "embedded",
	GconfList = [{id, Id}],
	DocRoot = er_configuration:webconsole_docroot(),
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
	[supervisor:start_child(easyrider_sup, Child) || Child <- Children],
	yaws_api:setconf(GC, SCList).
