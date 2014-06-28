-module(er_repository_web).
-include("yaws_api.hrl").
-export([start_link/0]).
-export([out/1]).
-record(upload, {fd, application, version, last}).

start_link() -> gen_server:start_link({local, er_repository_web}, er_repository_web, [], []).

out(A) when A#arg.state == undefined ->
	case A#arg.headers#headers.authorization of
		{Username, Password, _} ->
			case er_user_manager:authenticate(Username, Password) of
				{ok, _} ->
				    State = #upload{},
		    		multipart(A, State);
		    	_ -> err()
		    end;
		_ -> err()
	end;
out(A) ->
    multipart(A, A#arg.state).

err() ->
    {ehtml,
     {p, [], "error"}}.

err(Message) ->
    {ehtml,
     {p, [], atom_to_list(Message)}}.

multipart(A, State) ->
    Parse = yaws_api:parse_multipart_post(A),
    case Parse of
        {cont, Cont, Res} ->
        	case parse_content(Res, State) of
        		{ok, NewState} -> {get_more, Cont, NewState};
        		Other -> err(Other)
			end;
        {result, Res} ->
        	case parse_content(Res, State) of
        		{ok, _} -> {ehtml, {p,[], "File upload done"}};
        		Other -> err(Other)
        	end;
        {error, _Reason} ->
            err()
    end.

parse_content(Data, State) when State#upload.fd == undefined,
								State#upload.application /= undefined,
								State#upload.version /= undefined ->
	case er_repository:upload_version(State#upload.application, State#upload.version) of
		{ok, Fd} -> parse_content(Data, State#upload{fd = Fd});
		Other -> Other
	end;
parse_content([], State) -> {ok, State};
parse_content([{head, {Name, _}}, {body, Body} | Rest], State) ->
	NewState = case Name of
		"application" -> State#upload{application = Body};
		"version" -> State#upload{version = Body};
		"content" ->
			er_repository_upload:add_chunk(State#upload.fd, Body),
			er_repository_upload:done(State#upload.fd),
			State
		end,
	parse_content(Rest, NewState);
parse_content([{head, {Name, _}}, {part_body, PartBody} | Rest], State) ->
	Name = "upload",
	er_repository_upload:add_chunk(State#upload.fd, PartBody),
	parse_content(Rest, State);
parse_content([{part_body, PartBody} | Rest], State) ->
	er_repository_upload:add_chunk(State#upload.fd, PartBody),
	parse_content(Rest, State);
parse_content([{body, Body} | Rest], State) ->
	er_repository_upload:add_chunk(State#upload.fd, Body),
	er_repository_upload:done(State#upload.fd),
	parse_content(Rest, State).
