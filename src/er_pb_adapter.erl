-module(er_pb_adapter).
-export([encode/1]).

encode({app, _Name, _Properties} = App) ->
	messages_pb:encode_app(App).
