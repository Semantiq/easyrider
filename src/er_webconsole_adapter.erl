-module(er_webconsole_adapter).
-behaviour(gen_server).
-include("er_apps.hrl").
-include("er_repository.hrl").
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

start_link(Client) -> gen_server:start_link(er_webconsole_adapter, Client, []).

init(Client) ->
	{ok, Server} = er_api:start_link(self()),
	{ok, {Client, Server}}.

handle_cast(stop, State) -> {stop, shutdown, State};
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
handle_cast({snapshot, EventType, Data}, {Client, Server}) ->
	Json = {struct, [
		{"event", atom_to_list(EventType)},
		{"snapshot", true},
		{"data", [
			{struct, [
				{"key", event_key_json(EventType, Key)},
				{"value", event_value_json(EventType, Value)}
			]} || {Key, Value} <- Data
		]}
	]},
	er_webconsole_session:send_json(Client, Json),
	{noreply, {Client, Server}};
handle_cast({event, EventType, Key, Value}, {Client, Server}) ->
	Json = {struct, [
		{"event", atom_to_list(EventType)},
		{"snapshot", false},
		{"key", event_key_json(EventType, Key)},
		{"value", event_value_json(EventType, Value)}
	]},
	er_webconsole_session:send_json(Client, Json),
	{noreply, {Client, Server}};
handle_cast({versions, Versions}, {Client, Server}) ->
	Json = {struct, [
		{"event", "versions"},
		{"by_app", [
			{struct, [
				{"app", AppName},
				{"versions", [
					version_info_json(Version) || Version <- AppVersions
				]}
			]} || {AppName, AppVersions} <- Versions
		]}
	]},
	er_webconsole_session:send_json(Client, Json),
	{noreply, {Client, Server}};
handle_cast({new_version, Version}, {Client, Server}) ->
	Json = {struct, [
		{"event", "new_version"},
		{"version", version_info_json(Version)}
	]},
	er_webconsole_session:send_json(Client, Json),
	{noreply, {Client, Server}};
handle_cast({version_approved, Version}, {Client, Server}) ->
	Json = {struct, [
		{"event", "version_approved"},
		{"version", version_info_json(Version)}
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
parse_command("subscribe", {array, EventTypes}) ->
	{subscribe, [ list_to_atom(EventType) || EventType <- EventTypes ]};
parse_command("subscribe_versions", {struct, [{"limit", Limit}]}) ->
	{subscribe_versions, Limit}.

version_info_json(Version) -> {struct, [
	{"app", Version#version_info.app},
	{"number", Version#version_info.number},
	{"date", "date"}, % TODO: Version#version_info.date
	{"content_ref", io_lib:format("~p", [Version#version_info.content_ref])},
	{"approvals", [
		atom_to_list(Approval) || Approval <- Version#version_info.approvals
	]}
]}.

event_key_json(apps, AppName) -> AppName;
event_key_json(stages, {AppName, StageName}) -> {struct, [ {"app_name", AppName}, {"stage_name", StageName}]};
event_key_json(instances, {AppName, StageName, Id}) -> AppName ++ "-" ++ StageName ++ "-" ++ Id;
event_key_json(recommended_versions, {AppName, StageName}) -> {struct, [ {"app_name", AppName}, {"stage_name", StageName}]}.

event_value_json(apps, #app{app_name = Name, properties = Properties}) ->
	{struct, [
		{"name", Name},
		{"properties", properties_to_json(Properties)}
	]};
event_value_json(stages, #stage{app_name = AppName, stage_name = StageName, properties = Properties}) ->
	{struct, [
		{"app_name", AppName},
		{"stage_name", StageName},
		{"properties", properties_to_json(Properties)}
	]};
event_value_json(instances, #instance{app_name = AppName, stage_name = StageName, id = Id, properties = Properties}) ->
	{struct, [
		{"app_name", AppName},
		{"stage_name", StageName},
		{"id", Id},
		{"properties", properties_to_json(Properties)}
	]};
event_value_json(recommended_versions, {Version, Mode}) ->
	{struct, [
		{"version_info", version_info_json(Version)},
		{"mode", atom_to_list(Mode)}
	]}.

properties_to_json(Properties) -> 
	{array, [
		{struct, [{"type", "property"}, {"key", Key}, {"value", Value}]} || {property, Key, Value} <- Properties
	] ++ [
		{struct, [{"type", "rule"}, {"approvals", {array, Approvals}}]} || {rule, Approvals} <- Properties
	] ++ [
		{struct, [{"type", "release_window"}, {"times", {array, [
			{struct, [{"cron", CronExpression}]} || {cron, CronExpression} <- Times
		]}}]} || {release_window, Times} <- Properties
	]}.

%% Other gen_server callbacks
terminate(_, _State) -> ok.
handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.
code_change(_OldVersion, State, _) -> {ok, State}.
