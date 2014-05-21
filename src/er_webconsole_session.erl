-module(er_webconsole_session).
-include("yaws_api.hrl").
-export([send_json/2]).
-export([handle_message/2, init/1, terminate/2]).

init([_Request, Args]) ->
	{ok, Handler} = er_webconsole_adapter:start_link(self()),
	{ok, Handler}.

terminate(Reason, _) ->
	case Reason of
		{error, Error} -> io:format("Terminating: ~p~n", [Error]);
		_ -> dont_worry
	end,
    ok.

handle_message({text,Data}, Handler) ->
	{ok, Json} = json2:decode_string(binary_to_list(Data)),
	gen_server:cast(Handler, Json),
    {noreply, Handler};
handle_message({close, Status, Reason}, _State) ->
	io:format("Closing web session: ~p because ~p~n", [Status, Reason]),
	{close, user_disconnect}.

send_json(Socket, Message) ->
	yaws_api:websocket_send(Socket, {text, list_to_binary(json2:encode(Message))}).
