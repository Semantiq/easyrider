-module(er_release_manager).
-behaviour(gen_server).
-include("er_apps.hrl").
-include("er_repository.hrl").
-export([start_link/0, subscribe_recommended_versions/1, get_recommended_versions/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {recommended_versions = undefined, versions = undefined, apps = undefined, subscriptions = []}).

%% Interface

% @private
start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

% @doc Subscribe to version recommendations.
% Recommendations mean that it's safe to install given version of the AppName in given Stage immediately.
% The subsciber get the updates in the form of:
% {recommended_version,{{AppName,Stage},Version,mode()}}}
-spec subscribe_recommended_versions(Subscriber :: pid()) -> ok.
subscribe_recommended_versions(Pid) -> gen_server:cast({global, ?MODULE}, {subscribe_recommended_versions, Pid}), ok.

% @doc Get the list of recommended versions for each stage of each app.
% Please note that recommendations may not always be available (for example when {@link er_repository} is down).
-spec get_recommended_versions() -> {recommended_versions, [{{AppName::string(), Stage::string()}, Version::string()}]}.
get_recommended_versions() -> gen_server:call({global, ?MODULE}, {get_recommended_versions}).

%% gen_server

% @private
init(_Args) ->
	er_repository:subscribe_versions(self(), 1),
	er_event_bus:subscribe(self(), [apps, stages]),
	{ok, #state{}}.

% @private
handle_call({get_recommended_versions}, _From, State) ->
	{reply, {recommended_versions, State#state.recommended_versions}, State}.

% @private
handle_cast({subscribe_recommended_versions, Pid}, State) ->
	erlang:monitor(process, Pid),
	{noreply, State#state{subscriptions = [Pid | State#state.subscriptions]}};
% handle_cast({new_version, Version}, State) ->
% 	AppName = Version#version_info.app,
% 	Stages = case lists:keysearch(AppName, 2, State#state.apps) of
% 		{value, App} -> [ Stage#stage.name || Stage <- App#app.stages ];
% 		error -> []
% 	end,	
% 	Recommendations = [ {{AppName, Stage}, Version, immediate} || Stage <- Stages ],
% 	[ notify(Recommendation, State) || Recommendation <- Recommendations ],
% 	NewRecommendations = lists:foldl(fun({{_, Stage}, _, _}, NewRecommendations) ->
% 		orddict:store({AppName, Stage}, Version, NewRecommendations)
% 	end, State#state.recommended_versions, Recommendations),
% 	{noreply, State#state{recommended_versions = NewRecommendations}};
handle_cast({version_approved, _Version}, State) -> {noreply, State};
handle_cast({versions, Versions}, State) -> {noreply, make_recommendations(State#state{versions = Versions})};
handle_cast({snapshot, apps, Apps}, State) ->
	% TODO: {noreply, make_recommendations(State#state{apps = Apps})}.
	{noreply, State};
handle_cast({snapshot, stages, Stage}, State) -> {noreply, State};
handle_cast({event, apps, AppName, App}, State) -> {noreply, State};
handle_cast({event, stages, {AppName, StageName}, Stage}, State) -> {noreply, State}.

% @private
handle_info({'DOWN', _, process, Pid, _}, State) ->
	{noreply, State#state{subscriptions = lists:delete(Pid, State#state.subscriptions)}}.

%% helpers

% @private
% make_recommendations(State) when State#state.versions /=undefined, State#state.apps /= undefined ->
% 	Recommendations = [
% 		{{App#app.name, Stage#stage.name}, version_for(App#app.name, Stage#stage.name, State)}
% 		|| App <- State#state.apps, Stage <- App#app.stages ],
% 	State#state{recommended_versions = Recommendations};
make_recommendations(State) -> State.

% @private
version_for(App, _Stage, State) ->
	Versions = State#state.versions,
	case lists:keysearch(App, 1, Versions) of
		{value, {_, [AppVersion | _]}} -> AppVersion#version_info.number;
		_ -> undefined
	end.

% @private
notify(Recommendation, State) ->
	[ gen_server:cast(Subscriber, {recommended_version, Recommendation}) || Subscriber <- State#state.subscriptions ].

%% other gen_server

% @private
code_change(_, _, _) -> stub.
% @private
terminate(_, _) -> stub.
