#!/bin/sh

if [ "$mode" == "test" ]; then
	echo "Starting test $VERSION on $ID" >> test_app.log
	sleep 10
	echo "Test completed" >> test_app.log
	exit 0
else
	echo "Started app $VERSION on $ID" >> test_app.log
	while [ 1 = 1 ]; do
		date >> test_app.log
		echo "Hello test_app. My params: $*" >> test_app.log
		sleep 20
	done
fi
