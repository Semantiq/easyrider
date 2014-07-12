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

-ifndef(EVENT_PB_H).
-define(EVENT_PB_H, true).
-record(event, {
    timestamp = erlang:error({required, timestamp}),
    eventtype = erlang:error({required, eventtype}),
    key = []
}).
-endif.

-ifndef(LOGIN_PB_H).
-define(LOGIN_PB_H, true).
-record(login, {
    username = erlang:error({required, username}),
    password = erlang:error({required, password})
}).
-endif.

-ifndef(SUBSCRIBE_PB_H).
-define(SUBSCRIBE_PB_H, true).
-record(subscribe, {
    eventtypes = []
}).
-endif.

-ifndef(UNSUBSCRIBE_PB_H).
-define(UNSUBSCRIBE_PB_H, true).
-record(unsubscribe, {
    eventtypes = []
}).
-endif.

-ifndef(SETAPP_PB_H).
-define(SETAPP_PB_H, true).
-record(setapp, {
    app = erlang:error({required, app})
}).
-endif.

-ifndef(SETSTAGE_PB_H).
-define(SETSTAGE_PB_H, true).
-record(setstage, {
    stage = erlang:error({required, stage})
}).
-endif.

-ifndef(SETINSTANCE_PB_H).
-define(SETINSTANCE_PB_H, true).
-record(setinstance, {
    instance = erlang:error({required, instance})
}).
-endif.

-ifndef(DEPLOYINSTANCE_PB_H).
-define(DEPLOYINSTANCE_PB_H, true).
-record(deployinstance, {
    instanceid = erlang:error({required, instanceid}),
    versionnumber = erlang:error({required, versionnumber})
}).
-endif.

-ifndef(REMOVEINSTANCE_PB_H).
-define(REMOVEINSTANCE_PB_H, true).
-record(removeinstance, {
    instanceid = erlang:error({required, instanceid})
}).
-endif.

-ifndef(REMOVESTAGE_PB_H).
-define(REMOVESTAGE_PB_H, true).
-record(removestage, {
    app = erlang:error({required, app}),
    stage = erlang:error({required, stage})
}).
-endif.

-ifndef(REMOVEAPP_PB_H).
-define(REMOVEAPP_PB_H, true).
-record(removeapp, {
    app = erlang:error({required, app})
}).
-endif.

-ifndef(REJECTED_PB_H).
-define(REJECTED_PB_H, true).
-record(rejected, {
    message = erlang:error({required, message})
}).
-endif.

-ifndef(WELCOME_PB_H).
-define(WELCOME_PB_H, true).
-record(welcome, {
    role = erlang:error({required, role})
}).
-endif.

-ifndef(APPUPDATED_PB_H).
-define(APPUPDATED_PB_H, true).
-record(appupdated, {
    event = erlang:error({required, event}),
    app
}).
-endif.

-ifndef(STATEUPDATED_PB_H).
-define(STATEUPDATED_PB_H, true).
-record(stateupdated, {
    event = erlang:error({required, event}),
    stage
}).
-endif.

-ifndef(INSTANCEUPDATED_PB_H).
-define(INSTANCEUPDATED_PB_H, true).
-record(instanceupdated, {
    event = erlang:error({required, event}),
    instance
}).
-endif.

-ifndef(INSTANCEEVENT_PB_H).
-define(INSTANCEEVENT_PB_H, true).
-record(instanceevent, {
    event = erlang:error({required, event}),
    versionnumber
}).
-endif.

-ifndef(NEWVERSION_PB_H).
-define(NEWVERSION_PB_H, true).
-record(newversion, {
    event = erlang:error({required, event}),
    versionnumber = erlang:error({required, versionnumber})
}).
-endif.

-ifndef(VERSIONAPPROVED_PB_H).
-define(VERSIONAPPROVED_PB_H, true).
-record(versionapproved, {
    event = erlang:error({required, event}),
    approval = erlang:error({required, approval})
}).
-endif.

