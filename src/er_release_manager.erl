-module(er_release_manager).
-behaviour(gen_server).
-include("er_apps.hrl").
-include("er_repository.hrl").
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {versions = undefined, apps = undefined, stages = undefined, subscriptions = []}).

%% Interface

% @private
start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% gen_server

% @private
init(_Args) ->
	er_repository:subscribe_versions(self(), 1),
	{snapshot, apps, Apps} = er_event_bus:get_snapshot(apps),
	{ok, #state{apps = Apps, stages = undefined}}.

handle_cast({new_version, Version}, State) ->
	AppName = Version#version_info.app,
	{snapshot, stages, AllStages} = er_event_bus:get_snapshot(stages),
	Stages = [ Stage || {{ThisAppName, _StageName}, Stage} <- AllStages, ThisAppName == AppName ],
	Recommendations = [ {recommended_versions, {AppName, StageName}, {Version#version_info.number, immediate}} || #stage{stage_name = StageName} <- Stages ],
	[ er_event_bus:publish(Recommendation) || Recommendation <- Recommendations ],
	{noreply, State};
handle_cast({version_approved, _Version}, State) -> {noreply, State};
handle_cast({versions, Versions}, State) -> {noreply, make_recommendations(State#state{versions = Versions})}.

% @private
handle_info({'DOWN', _, process, Pid, _}, State) ->
	{noreply, State#state{subscriptions = lists:delete(Pid, State#state.subscriptions)}}.

%% helpers

% % @private
% make_recommendations(State) when State#state.versions /=undefined, State#state.apps /= undefined ->
% 	Recommendations = [
% 		{{App#app.name, Stage#stage.name}, version_for(App#app.name, Stage#stage.name, State)}
% 		|| App <- State#state.apps, Stage <- App#app.stages ],
% 	State#state{recommended_versions = Recommendations};
make_recommendations(State) -> State.

% % @private
% version_for(App, _Stage, State) ->
% 	Versions = State#state.versions,
% 	case lists:keysearch(App, 1, Versions) of
% 		{value, {_, [AppVersion | _]}} -> AppVersion#version_info.number;
% 		_ -> undefined
% 	end.

%% other gen_server

% @private
code_change(_, _, _) -> stub.
% @private
terminate(_, _) -> stub.
% @private
handle_call(_, _, _) -> stub.
