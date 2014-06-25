-module(er_repository).
-behaviour(gen_server).
-include("er_repository.hrl").
-export([start_link/0, versions/1, subscribe_versions/2, upload_version/2, upload_version_file/3, download_version_file/2, add_version/3, approve_version/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-record(state, {versions = [], subscriptions = []}).
-define(CHUNK_SIZE, 1024).

% Interface

start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
versions(Limit) -> gen_server:call({global, ?MODULE}, {versions, Limit}).
subscribe_versions(Pid, Limit) -> gen_server:cast({global, ?MODULE}, {subscribe_versions, Pid, Limit}).
upload_version(AppName, Number) -> gen_server:call({global, ?MODULE}, {upload_version, AppName, Number}).
upload_version_file(AppName, Number, FileName) ->
	{ok, Upload} = upload_version(AppName, Number),
	{ok, Fd} = file:open(FileName, [read]),
	stream_upload(Fd, Upload).
% app = _, number = _, date = _, approvals = _
download_version_file(#version_info{content_ref = ContentRef}, FileName) ->
	download_version_file(ContentRef, FileName);
download_version_file({Repo, Package}, FileName) ->
	{ok, Fd} = file:open(FileName, [write]),
	{ok, Download} = gen_server:call(Repo, {download, Package}),
	stream(Download, Fd).
add_version(AppName, Number, ContentRef) -> gen_server:cast({global, ?MODULE}, {add_version, AppName, Number, ContentRef}).
approve_version(AppName, Number, Approval) -> gen_server:cast({global, ?MODULE}, {approve_version, AppName, Number, Approval}).

%% gen_server

init(_Args) -> {ok, load_state()}.

handle_call({versions, Limit}, _From, State) ->
	Versions = get_versions(State, Limit),
	{reply, {versions, Versions}, State};
handle_call({upload_version, AppName, Number}, _From, State) ->
	% TODO: pick best instance of er_repository_storage
	Ret = case version_exists(AppName, Number, State) of
		true -> already_exists;
		false -> 
			{ok, Upload} = er_repository_storage:upload(AppName, Number),
			{ok, Upload}
	end,
	{reply, Ret, State}.

handle_cast({subscribe_versions, Pid, Limit}, State) ->
	erlang:monitor(process, Pid),
	gen_server:cast(Pid, {versions, get_versions(State, Limit)}),
	{noreply, State#state{subscriptions = [Pid | State#state.subscriptions]}};
handle_cast({add_version, AppName, Number, ContentRef}, State) ->
	case version_exists(AppName, Number, State) of
		true ->
			io:format("Ignoring upload of existing version: ~p v ~p~n", [AppName, Number]),
			{noreply, State};
		false ->
			Version = #version_info{app = AppName, date = now(), number = Number, content_ref = ContentRef},
			notify_subscribers(State, new_version, Version),
			Versions = orddict:update(AppName, fun(V) -> [Version | V] end, [Version], State#state.versions),
			NewState = State#state{versions = Versions},
			store_state(NewState),
			{noreply, NewState}
	end;
handle_cast({approve_version, AppName, Number, Approval}, State) ->
	Versions = orddict:fetch(AppName, State#state.versions),
	{value, Version} = lists:keysearch(Number, 3, Versions),
	ApprovedVersion = Version#version_info{approvals = [Approval | Version#version_info.approvals]},
	notify_subscribers(State, version_approved, ApprovedVersion),
	UpdatedVersions = lists:keyreplace(Number, 3, Versions, ApprovedVersion),
	NewState = State#state{versions = orddict:store(AppName, UpdatedVersions, State#state.versions)},
	store_state(NewState),
	{noreply, NewState}.

%% helpers

get_versions(State, Limit) -> [ {AppName, lists:sublist(Versions, Limit)} || {AppName, Versions} <- State#state.versions ].

notify_subscribers(State, Event, Version) -> [ gen_server:cast(Pid, {Event, Version}) || Pid <- State#state.subscriptions ].

repository_config() -> er_configuration:data_directory() ++ "/repository.config".

store_state(Data) ->
	% TODO: dedicated async process for storing stuff
	file:write_file(repository_config(),io_lib:fwrite("~p.\n",[Data#state{subscriptions = []}])).
load_state() ->
	case file:consult(repository_config()) of
		{ok, [State]} ->
			State#state{subscriptions = []};
		_ -> #state{}
	end.

version_exists(AppName, Number, State) ->
	case orddict:find(AppName, State#state.versions) of
		{ok, Versions} ->
			case lists:keysearch(Number, 3, Versions) of
				{value, _} -> true;
				_ -> false
			end;
		_ -> false
	end.

stream(From, To) ->
	case er_repository_download:get_chunk(From, ?CHUNK_SIZE) of
		{ok, Data} ->
			file:write(To, Data),
			stream(From, To);
		eof ->
			file:close(To)
	end.
stream_upload(From, To) ->
	case file:read(From, ?CHUNK_SIZE) of
		{ok, Data} ->
			er_repository_upload:add_chunk(To, Data),
			stream_upload(From, To);
		eof ->
			file:close(From),
			er_repository_upload:done(To)
	end.

%% Other gen_server callbacks

terminate(shutdown, _State) -> ok.
handle_info({'DOWN', _, process, Pid, _}, State) ->
	io:format("Removing ~p from subscriptions~n", [Pid]),
	{noreply, State#state{subscriptions = lists:delete(Pid, State#state.subscriptions)}};
handle_info(Info, State) ->
    io:format("Got info: ~p~n", [Info]),
    {noreply, State}.
code_change(_OldVersion, State, _) -> {ok, State}.
