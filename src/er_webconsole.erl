-module(er_webconsole).
-behaviour(gen_server).
-include("yaws_api.hrl").
-export([start_link/0]).
-export([out/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

start_link() ->
    gen_server:start_link({local, er_webconsole}, er_webconsole, [], []).

init(_Args) ->
	% Port = case application:get_env(easyrider, webconsole_port) of
	% 	{ok, FromConfig} -> FromConfig;
	% 	_ -> 8000
	% end,
	% Pid = yaws:start_embedded("web", [
	% 		{port, Port},
	% 		{listen, {0, 0, 0, 0}},
	% 		{appmods, [
	% 			{"/api", er_webconsole}
	% 		]}
	% 	]),
	{ok, []}.

handle_cast({write, Message}, State) ->
	io:fwrite(Message),
	{noreply, State}.

handle_call(_Message, _From, _State) -> unused.

terminate(shutdown, _State) ->
	yaws:stop(),
    ok.

handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.

code_change(OldVersion, State, Extra) ->
	io:format("Upgrading from version ~p. Extra: ~p~n", [OldVersion, Extra]),
    {ok, State}.

out(_A) -> {websocket, er_webconsole_session, []}.	
