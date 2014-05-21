-module(er_api).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

start_link(Client) -> gen_server:start_link(er_api, Client, []).

init(Client) -> {ok, {Client, not_authenticated}}.

handle_cast({login, Username, Password}, {Client, not_authenticated}) ->
	case {Username, Password} of
		{"test", "test"} -> 
			gen_server:cast(Client, {welcome, {"test", admin}}),
			{noreply, {Client, "test", admin}};
		_ ->
			gen_server:cast(Client, {bad_username_or_password, {}}),
			{noreply, {Client, not_authenticated}}
	end;
handle_cast({getApplications}, {Client, Username, Role}) ->
	{noreply, {Client, Username, Role}}.

%% Other gen_server callbacks

terminate(shutdown, _State) -> ok.
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.
code_change(_OldVersion, State, _) -> {ok, State}.
