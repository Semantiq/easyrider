-module(er_api_json).
-include("easyrider_pb.hrl").
-export([new/1, new/2, tell/2]).

-define(rd(Type), {Type, record_info(fields, Type)}).
-define(RECORD_TYPES, orddict:from_list([
	?rd(app), ?rd(stage), ?rd(instance), ?rd(event), ?rd(configuration),
	?rd(login),  ?rd(subscribe), ?rd(setapp), ?rd(setstage), ?rd(setinstance), ?rd(deployinstance),
	?rd(welcome), ?rd(rejected), ?rd(instanceevent), ?rd(snapshot), ?rd(snapshotentry),
	?rd(appupdated)
])).

%% interface

new(Login) -> new(Login, fun(Client, Json) -> gen_server:cast(Client, Json) end).
new(Login, Fun) -> er_api:new(json_to_record(Login), fun(Client, Record) -> Fun(Client, record_to_json(Record)) end).
tell(Session, Message) -> er_api:tell(Session, json_to_record(Message)).

%% helpers

json_to_record(Json) -> 
	{ok, Struct} = json2:decode_string(Json),
	struct_to_record(Struct).

struct_to_record({struct, Props}) ->
	TypeName = case lists:keysearch("type", 1, Props) of
		{value, {_, TypeValue}} -> TypeValue;
		_ -> throw(no_type_name)
	end,
	Type = list_to_atom(TypeName),
	Definition = case orddict:find(Type, ?RECORD_TYPES) of
		{ok, Def} -> Def;
		_ -> throw({unknown_type, Type})
	end,
	List = [Type] ++ lists:map(fun(Key) ->
		case lists:keysearch(atom_to_list(Key), 1, Props) of
			{value, {_, Value}} -> struct_to_record(Value);
			false -> undefined
		end
	end, Definition),
	list_to_tuple(List);
struct_to_record({array, Entries}) -> [ struct_to_record(Entry) || Entry <- Entries ];
struct_to_record(String) when is_list(String) -> String;
struct_to_record(null) -> undefined.

record_to_json(Record) -> json2:encode(record_to_struct(Record)).

record_to_struct(Record) when is_tuple(Record) ->
	[Type | Props] = tuple_to_list(Record),
	{ok, Definition} = orddict:find(Type, ?RECORD_TYPES),
	{struct, [{"type", atom_to_list(Type)}] ++ [
		{atom_to_list(Name), record_to_struct(Value)} || {Name, Value} <- lists:zip(Definition, Props)
	]};
record_to_struct(undefined) -> null;
record_to_struct([Entry | _] = List) when is_tuple(Entry) -> list_to_array(List);
record_to_struct(Number) when is_number(Number) -> Number;
record_to_struct(String) when is_list(String) -> String.

list_to_array(List) -> {array, [ record_to_struct(Entry) || Entry <- List ]}.
