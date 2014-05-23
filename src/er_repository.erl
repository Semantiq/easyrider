-module(er_repository).
-behaviour(gen_server).
-include("er_repository.hrl").
-export([start_link/0, versions/1, subscribe_versions/2, add_version/3, approve_version/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {versions = [], subscriptions = []}).

% Interface

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
versions(Limit) -> gen_server:call({global, ?MODULE}, {versions, Limit}).
subscribe_versions(Pid, Limit) -> gen_server:cast({global, ?MODULE}, {subscribe_versions, Pid, Limit}).
add_version(AppName, Number, Content) -> gen_server:cast({global, ?MODULE}, {add_version, AppName, Number, Content}).
approve_version(AppName, Number, Approval) -> gen_server:cast({global, ?MODULE}, {approve_version, AppName, Number, Approval}).

%% gen_server

init(_Args) -> {ok, #state{}}.

handle_call({versions, Limit}, _From, State) ->
	Versions = get_versions(State, Limit),
	{reply, {versions, Versions}, State}.

handle_cast({subscribe_versions, Pid, Limit}, State) ->
	erlang:monitor(process, Pid),
	gen_server:cast(Pid, {versions, get_versions(State, Limit)}),
	{noreply, State#state{subscriptions = [Pid | State#state.subscriptions]}};
handle_cast({add_version, AppName, Number, Content}, State) ->
	Version = #version_info{app = AppName, date = now(), number = Number, size = length(Content)},
	notify_subscribers(State, new_version, Version),
	Versions = orddict:update(AppName, fun(V) -> [Version | V] end, [Version], State#state.versions),
	{noreply, State#state{versions = Versions}};
handle_cast({approve_version, AppName, Number, Approval}, State) ->
	Versions = orddict:fetch(AppName, State#state.versions),
	{value, Version} = lists:keysearch(Number, 3, Versions),
	ApprovedVersion = Version#version_info{approvals = [Approval | Version#version_info.approvals]},
	notify_subscribers(State, version_approved, ApprovedVersion),
	UpdatedVersions = lists:keyreplace(Number, 3, Versions, ApprovedVersion),
	{noreply, State#state{versions = orddict:store(AppName, UpdatedVersions, State#state.versions)}}.

%% helpers

get_versions(State, Limit) -> [ {AppName, lists:sublist(Versions, Limit)} || {AppName, Versions} <- State#state.versions ].

notify_subscribers(State, Event, Version) -> [ gen_server:cast(Pid, {Event, Version}) || Pid <- State#state.subscriptions ].

%% Other gen_server callbacks

terminate(shutdown, _State) -> ok.
handle_info({'DOWN', _, process, Pid, _}, State) ->
	io:format("Removing ~p from subscriptions~n", [Pid]),
	{noreply, State#state{subscriptions = lists:delete(Pid, State#state.subscriptions)}};
handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.
code_change(_OldVersion, State, _) -> {ok, State}.
