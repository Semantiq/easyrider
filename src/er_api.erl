-module(er_api).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {client, username = none, role = not_authenticated}).

start_link(Client) -> gen_server:start_link(er_api, Client, []).

init(Client) -> {ok, #state{client = Client}}.

handle_cast({login, Username, Password}, #state{client = Client, role = not_authenticated} = State) ->
	case {Username, Password} of
		{"test", "test"} -> 
			gen_server:cast(Client, {welcome, {"test", admin}}),
			{noreply, State#state{username = "test", role = admin}};
		_ ->
			gen_server:cast(Client, {bad_username_or_password, {}}),
			{noreply, #state{client = Client}}
	end;
handle_cast({subscribe_apps}, State) ->
	er_apps:subscribe_apps(self()),
	{noreply, State};
handle_cast({subscribe_versions, Limit}, State) ->
	er_repository:subscribe_versions(self(), Limit),
	{noreply, State};
handle_cast({apps, Apps}, State) ->
	gen_server:cast(State#state.client, {apps, Apps}),
	{noreply, State};
handle_cast({versions, Versions}, State) ->
	gen_server:cast(State#state.client, {versions, Versions}),
	{noreply, State};
handle_cast({new_version, Version}, State) ->
	gen_server:cast(State#state.client, {new_version, Version}),
	{noreply, State};
handle_cast({version_approved, Version}, State) ->
	gen_server:cast(State#state.client, {version_approved, Version}),
	{noreply, State}.

%% Other gen_server callbacks

terminate(shutdown, _State) ->
	io:format("got terminate~n"),
	ok.
handle_call(_Req, _From, State) ->
	io:format("got a call~n"),
	{reply, ok, State}.
handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.
code_change(_OldVersion, State, _) -> {ok, State}.
