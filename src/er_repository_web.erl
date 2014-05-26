-module(er_repository_web).
-include("yaws_api.hrl").
-export([start_link/0]).
-export([out/1]).
-record(upload, {fd, application, version, last}).

start_link() -> gen_server:start_link({local, er_repository_web}, er_repository_web, [], []).

out(A) when A#arg.state == undefined ->
    State = #upload{},
    multipart(A, State);
out(A) ->
    multipart(A, A#arg.state).

err() ->
    {ehtml,
     {p, [], "error"}}.

multipart(A, State) ->
    Parse = yaws_api:parse_multipart_post(A),
    case Parse of
        {cont, Cont, Res} ->
        	{ok, NewState} = parse_content(Res, State),
			{get_more, Cont, NewState};
        {result, Res} ->
        	{ok, _} = parse_content(Res, State),
        	{ehtml, {p,[], "File upload done"}};
        {error, _Reason} ->
            err()
    end.

parse_content(Data, State) when State#upload.fd == undefined,
								State#upload.application /= undefined,
								State#upload.version /= undefined ->
	{ok, Fd} = er_repository:upload_version(State#upload.application, State#upload.version),
	parse_content(Data, State#upload{fd = Fd});
parse_content([], State) -> {ok, State};
parse_content([{head, {Name, _}}, {body, Body} | Rest], State) ->
	NewState = case Name of
		"application" -> State#upload{application = Body};
		"version" -> State#upload{version = Body};
		"upload" ->
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

% addFileChunk(A, [{part_body, Data}|Res], State) ->
%     addFileChunk(A, [{body, Data}|Res], State);

% addFileChunk(_A, [], State) when State#upload.last==true,
%                                  State#upload.filename /= undefined,
%                                  State#upload.fd /= undefined ->

%     io:format("TODO: Complete the upload here~n", []),
%     Res = {ehtml, {p, [], "result page here"}},
%     {done, Res};

% addFileChunk(A, [], State) when State#upload.last==true ->
%     {done, err()};

% addFileChunk(_A, [], State) ->
%     {cont, State};

% addFileChunk(A, [{head, {_Name, Opts}}|Res], State ) ->
%     case lists:keysearch("filename", 1, Opts) of
%         {value, {_, Fname0}} ->
%         	Fd = make_ref(),
%         	io:format("TODO: Start upload here~n", []),
% 		    case {ok, make_ref()} of
% 				{ok, Fd} ->
% 				    S2 = State#upload{filename = Fname0, fd = Fd},
% 			    	addFileChunk(A, Res, S2);
% 				Err ->
% 			    	{done, err()}
% 		    end;
% 		false ->
% 	            addFileChunk(A,Res,State)
%     end;

% addFileChunk(A, [{body, Data}|Res], State)
%   when State#upload.filename /= undefined ->
%   	%Result = file:write(State#upload.fd, Data)
%   	Result = ok,
%   	io:format("TODO: Write portion here~n", []),
%     case Result of
%         ok ->
%             addFileChunk(A, Res, State);
%         Err ->
%             {done, err()}
%     end.

% er_repository:add_version(Application, Version, FileContent),
% {redirect_local, "/"};
