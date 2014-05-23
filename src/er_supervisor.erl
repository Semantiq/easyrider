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
			permanent, 2000, worker, [er_repository]}
	],
	{ok, {{one_for_one, 3, 60}, lists:filter(fun should_start/1, Children)}}.

should_start({Child, _, _, _, _, _}) ->
	case Child of
		er_apps -> application:get_env(easyrider, run_apps) == {ok, true};
		er_repository -> application:get_env(easyrider, run_repository) == {ok, true};
		_ -> false
	end.
