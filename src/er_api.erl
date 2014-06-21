-module(er_api).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {client, username = none, role = not_authenticated}).

start_link(Client) -> gen_server:start_link(er_api, Client, []).

init(Client) -> {ok, #state{client = Client}}.

handle_cast({login, Username, Password}, #state{client = Client, role = not_authenticated} = State) ->
	case er_user_manager:authenticate(Username, Password) of
		{ok, Role} -> 
			gen_server:cast(Client, {welcome, {Username, Role}}),
			{noreply, State#state{username = Username, role = Role}};
		_ ->
			gen_server:cast(Client, {bad_username_or_password, {}}),
			{noreply, #state{client = Client}}
	end;
handle_cast({subscribe, EventTypes}, State) ->
	er_event_bus:subscribe(self(), EventTypes),
	{noreply, State};
handle_cast({subscribe_versions, Limit}, State) ->
	er_repository:subscribe_versions(self(), Limit),
	{noreply, State};
handle_cast({tell_instance, Id, Message}, State) ->
	er_node_manager:tell_instance(Id, Message),
	{noreply, State};
handle_cast({snapshot, EventType, Data}, State) ->
	gen_server:cast(State#state.client, {snapshot, EventType, Data}),
	{noreply, State};
handle_cast({event, EventType, AppName, Application}, State) ->
	gen_server:cast(State#state.client, {event, EventType, AppName, Application}),
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

terminate(Reason, _State) -> ok.
handle_call(_Req, _From, State) -> stub.
handle_info(Info, State) -> stub.
code_change(_OldVersion, State, _) -> stub.
