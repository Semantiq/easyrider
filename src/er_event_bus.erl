-module(er_event_bus).
-behaviour(gen_server).
-export([start_link/0, subscribe/2, unsubscribe/1, publish/1, get_snapshot/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).
-record(state, {snapshots = [], subscriptions = []}).

%% Interface
start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
subscribe(Pid, EventTypes) -> gen_server:cast({global, ?MODULE}, {subscribe, Pid, EventTypes}).
unsubscribe(Pid) -> gen_server:cast({global, ?MODULE}, {unsubscribe, Pid}).
publish({EventType, Key, Data}) -> gen_server:cast({global, ?MODULE}, {publish, EventType, Key, Data}).
get_snapshot(EventType) -> gen_server:call({global, ?MODULE}, {get_snapshot, EventType}).

%% gen_server

init(_) -> {ok, load_state()}.

handle_cast({subscribe, Pid, EventTypes}, State) ->
	erlang:monitor(process, Pid),
	Snapshots = [ {EventType, get_snapshot(EventType, State)} || EventType <- EventTypes ],
	[ gen_server:cast(Pid, {snapshot, EventType, Snapshot}) || {EventType, Snapshot} <- Snapshots ],
	{noreply, State#state{subscriptions = orddict:store(Pid, EventTypes, State#state.subscriptions)}};
handle_cast({unsubscribe, UnsubscribingPid}, State) ->
	{noreply, State#state{subscriptions = lists:filter(fun({Pid, _}) -> Pid /= UnsubscribingPid end, State#state.subscriptions)}};
handle_cast({publish, EventType, Key, remove}, State) ->
	Snapshot = get_snapshot(EventType, State),
	UpdatedSnapshot = orddict:erase(Key, Snapshot),
	UpdatedState = State#state{snapshots = orddict:store(EventType, UpdatedSnapshot, State#state.snapshots)},
	notify(EventType, Key, remove, UpdatedState),
	{noreply, UpdatedState};
handle_cast({publish, EventType, Key, Data}, State) ->
	Snapshot = get_snapshot(EventType, State),
	UpdatedSnapshot = orddict:store(Key, Data, Snapshot),
	UpdatedState = State#state{snapshots = orddict:store(EventType, UpdatedSnapshot, State#state.snapshots)},
	notify(EventType, Key, Data, UpdatedState),
	{noreply, UpdatedState}.

handle_call({get_snapshot, EventType}, _From, State) ->
	Snapshot = get_snapshot(EventType, State),
	{reply, {snapshot, EventType, Snapshot}, State}.

handle_info({'DOWN', _, process, Pid, _}, State) ->
	io:format("Removing ~p from subscriptions~n", [Pid]),
	{noreply, State#state{subscriptions = orddict:erase(Pid, State#state.subscriptions)}}.

%% helpers

get_snapshot(EventType, State) ->
	case orddict:find(EventType, State#state.snapshots) of
		{ok, Snapshot} -> Snapshot;
		_ -> []
	end.

notify(EventType, Key, Data, State) ->
	store_state(State),
	[ gen_server:cast(Subscriber, {event, EventType, Key, Data}) ||
			{Subscriber, EventTypes} <- State#state.subscriptions,
			SubscribedEvent <- EventTypes,
			SubscribedEvent == EventType ].

store_state(#state{snapshots = Snapshots}) ->
	% TODO: dedicated async process for storing stuff
	file:write_file([er_configuration:data_directory(), "/state.config"],io_lib:fwrite("~p.\n",[Snapshots])).

load_state() ->
	case file:consult(lists:concat([er_configuration:data_directory(), "/state.config"])) of
		{ok, [Data]} ->
			#state{snapshots = Data};
		_ -> #state{}
	end.

%% other gen_server

terminate(_, _) -> ok.
code_change(_, _, _) -> ok.
