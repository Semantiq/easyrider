-module(er_repository_download).
-behaviour(gen_server).
-export([start_link/1, get_chunk/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-define(TIMEOUT, 30000).

%% Interface

start_link(Package) -> gen_server:start_link(?MODULE, [Package], []).
get_chunk(Pid, Size) -> gen_server:call(Pid, {get_chunk, Size}).

%% gen_server

init(Package) ->
	{ok, Fd} = file:open([er_configuration:repo_directory(), Package], [read]),
	{ok, Fd, ?TIMEOUT}.

handle_call({get_chunk, Size}, _From, Fd) ->
	case file:read(Fd, Size) of
		{ok, Data} -> {reply, {ok, Data}, Fd, ?TIMEOUT};
		eof ->
			file:close(Fd),
			{stop, normal, eof, undefined}
	end.

handle_info(timeout, Fd) ->
	file:close(Fd),
	{stop, "Download timeout", undefined}.

%% other gen_server

handle_cast(_, _) -> stub.
terminate(_, _) -> stub.
code_change(_, _, _) -> stub.
