-module(er_repository_storage).
-behaviour(gen_server).
-export([start_link/0, download/1, upload/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

%% Interface

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
download(Package) -> gen_server:call(?MODULE, {download, Package}).
upload(AppName, Package) -> er_repository_upload:start_link(AppName, Package).

%% gen_server

init(_Args) -> {ok, {}}.
handle_call({download, Package}, _From, State) ->
	{ok, Download} = er_repository_download:start_link(Package),
	{reply, {ok, Download}, State}.

%% other gen_server

handle_cast(_, _) -> stub.
handle_info(_, _) -> stub.
terminate(_, _) -> stub.
code_change(_, _, _) -> stub.
