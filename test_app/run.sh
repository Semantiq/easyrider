#!/bin/sh

if [ "$mode" == "test" ]; then
	echo "Starting test $VERSION on $ID" >> test_app.log
	sleep 10
	echo "Test completed" >> test_app.log
	exit $exitCode
else
	echo "Started app $VERSION on $ID" >> test_app.log
	if [ "$port" == "" ]; then
		echo "No port defined"
		exit 2
	fi
	while true; do { echo -e 'HTTP/1.1 200 OK\r\n'; echo "Hello world $VERSION"; } | nc -l $port; done
fi
