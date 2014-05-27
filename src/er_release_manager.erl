-module(er_release_manager).
-behaviour(gen_server).
-include("er_apps.hrl").
-include("er_repository.hrl").
-export([start_link/0, subscribe/1, get/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {recommended_versions = undefined, versions = undefined, apps = undefined, subscriptions = []}).

%% Interface

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
subscribe(Pid) -> gen_server:cast({global, ?MODULE}, {subscribe, Pid}).
get() -> gen_server:call({global, ?MODULE}, {get}).

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
	AppName = Version#version_info.app,
	Stages = case lists:keysearch(AppName, 2, State#state.apps) of
		{value, App} -> [ Stage#stage.name || Stage <- App#app.stages ];
		error -> []
	end,	
	Recommendations = [ {{AppName, Stage}, Version#version_info.number, immediate} || Stage <- Stages ],
	[ notify(Recommendation, State) || Recommendation <- Recommendations ],
	NewRecommendations = lists:foldl(fun({{_, Stage}, Number, _}, NewRecommendations) ->
		orddict:store({AppName, Stage}, Number, NewRecommendations)
	end, State#state.recommended_versions, Recommendations),
	{noreply, State#state{recommended_versions = NewRecommendations}};
handle_cast({version_approved, _Version}, State) -> {noreply, State};
handle_cast({versions, Versions}, State) -> {noreply, make_recommendations(State#state{versions = Versions})};
handle_cast({apps, Apps}, State) -> {noreply, make_recommendations(State#state{apps = Apps})}.

handle_info({'DOWN', _, process, Pid, _}, State) ->
	{noreply, State#state{subscriptions = lists:delete(Pid, State#state.subscriptions)}}.

%% helpers

make_recommendations(State) when State#state.versions /=undefined, State#state.apps /= undefined ->
	Recommendations = [
		{{App#app.name, Stage#stage.name}, version_for(App#app.name, Stage#stage.name, State)}
		|| App <- State#state.apps, Stage <- App#app.stages ],
	State#state{recommended_versions = Recommendations};
make_recommendations(State) -> State.

version_for(App, _Stage, State) ->
	Versions = State#state.versions,
	case lists:keysearch(App, 1, Versions) of
		{value, {_, [AppVersion | _]}} -> AppVersion#version_info.number;
		_ -> undefined
	end.

notify(Recommendation, State) ->
	[ gen_server:cast(Subscriber, {recommended_version, Recommendation}) || Subscriber <- State#state.subscriptions ].

%% other gen_server

code_change(_, _, _) -> stub.
terminate(_, _) -> stub.
