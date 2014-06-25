-module(er_configuration).
-export([data_directory/0, repo_directory/0, instances_directory/0, webconsole_docroot/0]).

data_directory() ->
	case application:get_env(easyrider, data_directory) of
		{ok, DataDirectory} -> DataDirectory;
		_ -> "data/"
	end.

webconsole_docroot() ->
	case application:get_env(easyrider, webconsole_docroot) of
		{ok, DataDirectory} -> DataDirectory;
		_ -> "web/"
	end.

repo_directory() -> lists:concat([data_directory(), "repository/"]).

instances_directory() -> lists:concat([data_directory(), "instances/"]).
