-module(er_supervisor).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	Children = [
		{er_apps,
			{er_apps, start_link, []},
			permanent, 1000, worker, [er_apps]},
		{er_repository,
			{er_repository, start_link, []},
			permanent, 2000, worker, [er_repository]},
		{er_repository_storage,
			{er_repository_storage, start_link, []},
			permanent, 2000, worker, [er_repository_storage, er_repository_download, er_repository_upload]},
		{er_release_manager,
			{er_release_manager, start_link, []},
			permanent, 2000, worker, [er_release_manager]},
		{er_node_agent,
			{er_node_agent, start_link, []},
			permanent, 5000, worker, [er_node_agent]},
		{er_orchestrator,
			{er_orchestrator, start_link, []},
			permanent, 2000, worker, [er_orchestrator]}
	],
	{ok, {{one_for_one, 3, 60}, lists:filter(fun should_start/1, Children)}}.

should_start({Child, _, _, _, _, _}) ->
	case Child of
		er_apps -> application:get_env(easyrider, run_apps) == {ok, true};
		er_repository -> application:get_env(easyrider, run_repository) == {ok, true};
		er_repository_storage -> application:get_env(easyrider, run_repository_storage) == {ok, true};
		er_release_manager -> application:get_env(easyrider, run_release_manager) == {ok, true};
		er_node_agent -> true;
		er_orchestrator -> application:get_env(easyrider, run_orchestrator) == {ok, true};
		_ -> false
	end.
