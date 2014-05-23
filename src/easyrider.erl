-module(easyrider).
-behaviour(application).
-export([start/2, stop/1]).

start(_Mode, _Args) -> er_supervisor:start_link().
stop(_State) -> ok.
