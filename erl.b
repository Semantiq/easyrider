erl -config config/b.config -pa deps/yaws/ebin -pa deps/protobuffs/ebin -pa ebin -sname b -eval "application:start(easyrider)"
