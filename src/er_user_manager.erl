-module(er_user_manager).
-export([start_link/0, authenticate/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3, handle_info/2]).

%% Interface

% @private
start_link() -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
authenticate(Username, Password) -> gen_server:call({global, ?MODULE}, {authenticate, Username, Password}).

%% gen_server

init(_Args) ->
	{ok, UserConfig} = application:get_env(easyrider, users),
	UserList = [ {Username, {Password, Role}} || {Username, Password, Role} <- UserConfig],
	{ok, orddict:from_list(UserList)}.

handle_call({authenticate, Username, Password}, _From, Users) ->
	PasswordHash = crypto:hash(md5, Password),
	case orddict:find(Username, Users) of
		{ok, {PasswordHash, Role}} -> {reply, {ok, Role}, Users};
		_ -> {reply, bad_username_or_password, Users}
	end.

%% other gen_server

handle_cast(_, _) -> stub.
handle_info(_, _) -> stub.
terminate(_, _) -> stub.
code_change(_, _, _) -> stub.
