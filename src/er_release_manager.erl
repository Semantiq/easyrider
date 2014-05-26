-module(er_release_manager).
-behaviour(gen_server).
-include("er_apps.hrl").
-include("er_repository.hrl").
-export([start_link/0, subscribe/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {recommended_versions = undefined, versions = undefined, apps = undefined, subscriptions = []}).

%% Interface

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
subscribe(Pid) -> gen_server:cast(?MODULE, {subscribe, Pid}).

%% gen_server

init(_Args) ->
	er_repository:subscribe_versions(self(), 1),
	er_apps:subscribe_apps(self()),
	{ok, #state{}}.

handle_call({get}, _From, State) ->
	{reply, {recommended_versions, State#state.recommended_versions}, State}.

handle_cast({subscribe, Pid}, State) ->
	erlang:monitor(process, Pid),
	{noreply, State#state{subscriptions = [Pid | State#state.subscriptions]}};
handle_cast({new_version, Version}, State) ->
	io:format("recommend: ~p~n", [Version]),
	{noreply, State};
handle_cast({version_approved, Version}, State) ->
	io:format("recommend: ~p~n", [Version]),
	{noreply, State};
handle_cast({versions, Versions}, State) ->
	io:format("recommend: ~p~n", [Versions]),
	{noreply, make_recommendations(State#state{versions = Versions})};
handle_cast({apps, Apps}, State) ->
	io:format("apps: ~p~n", [Apps]),
	{noreply, make_recommendations(State#state{apps = Apps})}.

handle_info({'DOWN', _, process, Pid, _}, State) ->
	io:format("Removing ~p from subscriptions~n", [Pid]),
	{noreply, State#state{subscriptions = lists:delete(Pid, State#state.subscriptions)}}.

%% helpers

make_recommendations(State) when State#state.versions /=undefined, State#state.apps /= undefined ->
	Recommendations = [
		{App#app.name, Stage#stage.name, version_for(App#app.name, Stage#stage.name, State)}
		|| App <- State#state.apps, Stage <- App#app.stages ],
	State#state{recommended_versions = Recommendations};
make_recommendations(State) -> State.

version_for(App, _Stage, State) ->
	Versions = State#state.versions,
	case lists:keysearch(App, 1, Versions) of
		{value, {_, [AppVersion | _]}} -> AppVersion#version_info.number;
		_ -> undefined
	end.

%% other gen_server

code_change(_, _, _) -> stub.
terminate(_, _) -> stub.
