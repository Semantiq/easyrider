-module(easyrider).
-behaviour(application).
-export([start/2, stop/1]).

start(_Mode, _Args) ->
	case application:get_env(?MODULE, run_apps) of
		{ok, true} -> er_apps:start_link();
		_ -> ok
	end,
	er_webconsole:start_link().

stop(_State) ->
	ok.
