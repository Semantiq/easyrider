-ifndef(EASYRIDER_H).
-define(EASYRIDER_H, true).

-record(app, {
    name = erlang:error({required, name}),
    configuration = erlang:error({required, configuration})
}).

-record(stage, {
    app = erlang:error({required, app}),
    stage = erlang:error({required, stage}),
    configuration = erlang:error({required, configuration})
}).

-record(instance, {
    app = erlang:error({required, app}),
    stage = erlang:error({required, stage}),
    id = erlang:error({required, id}),
    nodeid = erlang:error({required, nodeid}),
    configuration = erlang:error({required, configuration})
}).

-record(configuration, {
    properties = [],
    wrapperproperties = []
}).

-record(property, {
    name = erlang:error({required, name}),
    value = erlang:error({required, value})
}).

-record(event, {
    timestamp = erlang:error({required, timestamp}),
    eventtype = erlang:error({required, eventtype}),
    key = []
}).

-record(login, {
    username = erlang:error({required, username}),
    password = erlang:error({required, password})
}).

-record(subscribe, {
    eventtypes = []
}).

-record(unsubscribe, {
    eventtypes = []
}).

-record(setapp, {
    app = erlang:error({required, app})
}).

-record(setstage, {
    stage = erlang:error({required, stage})
}).

-record(setinstance, {
    instance = erlang:error({required, instance})
}).

-record(deployinstance, {
    instanceid = erlang:error({required, instanceid}),
    versionnumber = erlang:error({required, versionnumber})
}).

-record(removeinstance, {
    instanceid = erlang:error({required, instanceid})
}).

-record(removestage, {
    app = erlang:error({required, app}),
    stage = erlang:error({required, stage})
}).

-record(removeapp, {
    app = erlang:error({required, app})
}).

-record(rejected, {
    message = erlang:error({required, message})
}).

-record(welcome, {
    role = erlang:error({required, role})
}).

-record(appupdated, {
    event = erlang:error({required, event}),
    data
}).

-record(stateupdated, {
    event = erlang:error({required, event}),
    stage
}).

-record(instanceupdated, {
    event = erlang:error({required, event}),
    instance
}).

-record(instanceevent, {
    event = erlang:error({required, event}),
    versionnumber
}).

-record(newversion, {
    event = erlang:error({required, event}),
    versionnumber = erlang:error({required, versionnumber})
}).

-record(versionapproved, {
    event = erlang:error({required, event}),
    approval = erlang:error({required, approval})
}).

%% Snapshots

-record(snapshot, {
    eventtype = erlang:error({required, eventtype}),
    data = []
}).

-record(snapshotentry, {
    key = [],
    details = erlang:error({required, details})
}).

-endif.
