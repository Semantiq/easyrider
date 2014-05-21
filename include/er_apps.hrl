-record{state, {apps = [], stages = [], instances = []}}.

-record(app, {name, stages = []}).
-record(stage, {name, instances = []}).
-record(instance, {id, node}).
