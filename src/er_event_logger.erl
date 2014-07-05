-module(er_event_logger).
-behaviour(gen_server).
-export([start_link/0, log_event/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

%% Interface

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
log_event(Event) -> gen_server:abcast(?MODULE, {log_event, Event}).

%% gen_server

init(_Args) ->
	LogFile = "event.log",
	{ok, F} = file:open(LogFile, [write]),
	{ok, F}.
handle_cast({log_event, Event}, F) ->
	file:write(F, io_lib:fwrite("~p.\n",[Event])),
	{noreply, F}.
terminate(_Reason, F) ->
	file:close(F).

%% other gen_server

handle_call(_, _, _) -> stub.
code_change(_, _, _) -> stub.
handle_info(_, _) -> stub.
