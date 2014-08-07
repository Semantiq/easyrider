-module(er_webconsole_session).
-include("yaws_api.hrl").
-export([send_json/2]).
-export([handle_message/2, init/1, terminate/2]).

init([_Request, _Args]) ->
	{ok, not_logged_in}.

terminate(Reason, _) ->
	case Reason of
		{error, Error} -> io:format("Terminating: ~p~n", [Error]);
		_ -> dont_worry
	end,
    ok.

handle_message({text,Data}, not_logged_in) ->
	{ok, Session} = er_api_json:new(binary_to_list(Data), fun(Client, Json) ->
		yaws_api:websocket_send(Client, {text, list_to_binary(Json)})
	end),
    {noreply, {session, Session}};
handle_message({text,Data}, {session, Session}) ->
	er_api_json:tell(Session, binary_to_list(Data)),
	{noreply, {session, Session}};
handle_message({close, _Status, _Reason}, Handler) ->
	gen_server:cast(Handler, stop),
	{close, user_disconnect}.

send_json(Socket, Message) ->
	yaws_api:websocket_send(Socket, {text, list_to_binary(json2:encode(Message))}).
