-module(er_webconsole_adapter).
-behaviour(gen_server).
-include("er_apps.hrl").
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

start_link(Client) -> gen_server:start_link(er_webconsole_adapter, Client, []).

init(Client) ->
	{ok, Server} = er_api:start_link(self()),
	{ok, {Client, Server}}.

handle_cast({struct, Fields}, {Client, Server}) ->
	gen_server:cast(Server, parse_message(Fields)),
	{noreply, {Client, Server}};
handle_cast({welcome, {Username, Role}}, {Client, Server}) ->
	Json = {struct, [
		{"event", "welcome"},
		{"username", Username},
		{"role", atom_to_list(Role)}
	]},
	er_webconsole_session:send_json(Client, Json),
	{noreply, {Client, Server}};
handle_cast({apps, Apps}, {Client, Server}) ->
	Json = {struct, [
		{"event", "apps"},
		{"apps", [			
			{struct, [
				{"name", App#app.name},
				{"stages", [
					{struct, [
						{"name", Stage#stage.name},
						{"instances", [
							{struct, [
								{"id", Instance#instance.id},
								{"node", atom_to_list(Instance#instance.node)}
							]} || Instance <- Stage#stage.instances
						]}
					]} || Stage <- App#app.stages
				]}
			]} || App <- Apps
		]}
	]},
	er_webconsole_session:send_json(Client, Json),
	{noreply, {Client, Server}}.

parse_message(Fields) ->
	{value, {"command", Command}} = lists:keysearch("command", 1, Fields),
	{value, {"body", Body}} = lists:keysearch("body", 1, Fields),
	parse_command(Command, Body).

parse_command("login", {struct, Fields}) ->
	{value, {"username", Username}} = lists:keysearch("username", 1, Fields),
	{value, {"password", Password}} = lists:keysearch("password", 1, Fields),
	{login, Username, Password};
parse_command("subscribe_apps", {struct, []}) ->
	{subscribe_apps}.

%% Other gen_server callbacks
terminate(shutdown, _State) -> ok.
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.
code_change(_OldVersion, State, _) -> {ok, State}.
