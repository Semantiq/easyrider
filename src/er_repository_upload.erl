-module(er_repository_upload).
-behaviour(gen_server).
-export([start_link/2, add_chunk/2, done/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

-define(TIMEOUT, 30000).
-record(upload, {application, version, fd, package}).

%% Interface

start_link(AppName, Number) -> gen_server:start_link(?MODULE, [AppName, Number], []).
add_chunk(Upload, Content) -> gen_server:cast(Upload, {add_chunk, Content}).
done(Upload) -> gen_server:cast(Upload, {done}).

%% gen_server

init([AppName, Number]) ->
	PackageName = io_lib:format("~s-~s.zip", [AppName, Number]),
	RepoDirectory = er_configuration:repo_directory(),
	filelib:ensure_dir(RepoDirectory),
	{ok, Fd} = file:open([RepoDirectory, PackageName], [write]),
	{ok, #upload{application = AppName, version = Number, fd = Fd, package = PackageName}, ?TIMEOUT}.

handle_cast({add_chunk, Content}, State) ->
	file:write(State#upload.fd, Content),
	{noreply, State, ?TIMEOUT};
handle_cast({done}, State) ->
	file:close(State#upload.fd),
	er_repository:add_version(State#upload.application, State#upload.version, {{er_repository_storage, node()}, State#upload.package}),
	{stop, normal, State}.

handle_info(timeout, State) -> {stop, "Upload timed out", State}.

terminate(normal, _) -> ok;
terminate(Reason, State) -> io:format("TODO: upload shutdown (~p), clean-up ~p~n", [Reason, State]).

%% other gen_server

handle_call(_, _, _) -> stub.
code_change(_, _, _) -> stub.
