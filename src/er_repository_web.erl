-module(er_repository_web).
-behaviour(gen_server).
-include("yaws_api.hrl").
-export([start_link/0]).
-export([out/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

start_link() -> gen_server:start_link({local, er_repository_web}, er_repository_web, [], []).

init(_Args) -> {ok, []}.

out(A) ->
	Options = [no_temp_file],
	case yaws_multipart:read_multipart_form(A, Options) of
		{done, Params} ->
			io:format("Params: ~p~n", [Params]),
			{ok, [{"filename", FileName}, {value, FileContent} | _]} = dict:find("data", Params),
			{redirect_local, "/"};
		{error, Reason} ->
			io:format("Error reading multipart form: ~p~n", [Reason])
	end.

% gen_server stubs

handle_cast({write, Message}, State) -> {noreply, State}.
handle_call(_Message, _From, _State) -> unused.
terminate(shutdown, _State) -> ok.
handle_info(Info, State) -> {noreply, State}.
code_change(OldVersion, State, Extra) -> {ok, State}.
