-ifndef(APP_PB_H).
-define(APP_PB_H, true).
-record(app, {
    name = erlang:error({required, name}),
    configuration = erlang:error({required, configuration})
}).
-endif.

-ifndef(STAGE_PB_H).
-define(STAGE_PB_H, true).
-record(stage, {
    app = erlang:error({required, app}),
    stage = erlang:error({required, stage}),
    configuration = erlang:error({required, configuration})
}).
-endif.

-ifndef(INSTANCE_PB_H).
-define(INSTANCE_PB_H, true).
-record(instance, {
    app = erlang:error({required, app}),
    stage = erlang:error({required, stage}),
    id = erlang:error({required, id}),
    nodeid = erlang:error({required, nodeid}),
    configuration = erlang:error({required, configuration})
}).
-endif.

-ifndef(CONFIGURATION_PB_H).
-define(CONFIGURATION_PB_H, true).
-record(configuration, {
    properties = [],
    wrapperproperties = []
}).
-endif.

-ifndef(PROPERTY_PB_H).
-define(PROPERTY_PB_H, true).
-record(property, {
    name = erlang:error({required, name}),
    value = erlang:error({required, value})
}).
-endif.

