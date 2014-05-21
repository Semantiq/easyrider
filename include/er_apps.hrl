-record(state, {apps = [], stages = [], instances = []}).

-record(app, {name, properties = [], stages = []}).
-record(stage, {name, properties = [], instances = []}).
-record(instance, {id, node, properties = []}).
-record(property, {key, value}).
