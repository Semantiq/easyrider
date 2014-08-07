-module(er_api).
-include("easyrider_pb.hrl").
-include("er_repository.hrl").
-behaviour(gen_server).
-export([new/1, new/2, tell/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {client, username = none, role = not_authenticated, transformation}).

%% interface

new(Login) -> new(Login, fun(Client, Message) -> gen_server:cast(Client, Message) end).
new(#login{username = Username, password = Password}, Transformation) ->
	case er_user_manager:authenticate(Username, Password) of
		{ok, Role} -> 
			State = #state{username = Username, role = Role, client = self(), transformation = Transformation},
			tell_client(State, #welcome{role = atom_to_list(Role)}),
			gen_server:start_link(?MODULE, State, []);
		_ ->
			#rejected{message = "bad_username_or_password"}
	end.
tell(Session, Command) -> gen_server:cast(Session, Command).

%% gen_server

init(State) -> {ok, State}.

handle_cast(#subscribe{eventtypes = EventTypes}, State) ->
	EventBusEvents = [list_to_atom(EventType) || EventType <- EventTypes, EventType /= "versions"],
	case lists:any(fun(E) -> E == "versions" end, EventTypes) of
		true -> er_repository:subscribe_versions(self(), 10);
		_ -> dont_subscribe_to_versions
	end,
	er_event_bus:subscribe(self(), EventBusEvents),
	{noreply, State};
handle_cast(#setapp{app = App}, State) ->
	er_apps:set_app(App),
	{noreply, State};
handle_cast(#setstage{stage = Stage}, State) ->
	er_apps:set_stage(Stage),
	{noreply, State};
handle_cast(#setinstance{instance = Instance}, #state{client = Client} = State) ->
	case er_apps:set_instance(Instance) of
		ok -> ok;
		Reason -> gen_server:cast(Client, #rejected{message = Reason})
	end,
	{noreply, State};
handle_cast(#deployinstance{instanceid = Id, versionnumber = VersionNumber}, State) ->
	er_apps:tell_instance(Id, {deploy, VersionNumber}),
	{noreply, State};
handle_cast(#removeinstance{instanceid = Id}, State) ->
	er_apps:tell_instance(Id, remove),
	{noreply, State};
handle_cast(#removestage{app = App, stage = Stage}, State) ->
	ok = er_apps:remove_stage(App, Stage),
	{noreply, State};
handle_cast(#removeapp{app = App}, State) ->
	ok = er_apps:remove_app(App),
	{noreply, State};

handle_cast({event, instance_events, Id, {Event, Version}}, State) ->
	tell_client(State, #instanceevent{event = #event{timestamp = timestamp_now(), eventtype = atom_to_list(Event), key = [Id]}, versionnumber = Version}),
	{noreply, State};
handle_cast({event, instance_events, Id, remove}, State) ->
	tell_client(State, #instanceevent{event = #event{timestamp = timestamp_now(), eventtype = "remove", key = [Id]}}),
	{noreply, State};
handle_cast({new_version, #version_info{app = App, number = Number}}, State) ->
	tell_client(State, #newversion{event = #event{timestamp = timestamp_now(), eventtype = "new_version", key = [App]}, versionnumber = Number}),
	{noreply, State};
handle_cast({event, apps, AppName, remove}, State) ->
	tell_client(State, #appupdated{event = #event{timestamp = timestamp_now(), eventtype = "remove", key = [AppName]}}),
	{noreply, State};
handle_cast({event, apps, AppName, App}, State) ->
	tell_client(State, #appupdated{event = #event{timestamp = timestamp_now(), eventtype = "apps", key = [AppName]}, app = App}),
	{noreply, State};
handle_cast({event, instances, {AppName, StageName, Id}, remove}, State) ->
	tell_client(State, #instanceupdated{event = #event{timestamp = timestamp_now(), eventtype = "remove", key = [AppName, StageName, Id]}}),
	{noreply, State};
handle_cast({event, instances, {AppName, StageName, Id}, Instance}, State) ->
	tell_client(State, #instanceupdated{event = #event{timestamp = timestamp_now(), eventtype = "instances", key = [AppName, StageName, Id]}, instance = Instance}),
	{noreply, State};

handle_cast({snapshot, EventType, Data}, State) ->
	tell_client(State, #snapshot{eventtype = atom_to_list(EventType), data = [ #snapshotentry{key = translate_key(EventType, Key), details = Details} || {Key, Details} <- Data]}),
	{noreply, State};
handle_cast({versions, Versions}, State) ->
	%% TODO: handle versions snapshot
	gen_server:cast(State#state.client, {versions, Versions}),
	{noreply, State};
handle_cast({version_approved, Version}, State) ->
	gen_server:cast(State#state.client, {version_approved, Version}),
	{noreply, State}.

%% helpers

%% TODO: events keys should be lists internally as well
translate_key(instance_events, Id) -> [Id].

tell_client(#state{client = Client, transformation = Transformation}, Message) ->
	Transformation(Client, Message).

timestamp_now() ->
	{MegaSecs, Secs, MicroSecs} = now(),
	MegaSecs * 1000000000 + Secs * 1000 + MicroSecs div 1000.


%% Other gen_server callbacks

terminate(_, _) -> stub.
handle_call(_, _, _) -> stub.
handle_info(_, _) -> stub.
code_change(_, _, _) -> stub.
