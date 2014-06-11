#!/bin/sh

while [ 1 = 1 ]; do
	date >> test_app.log
	echo "Hello test_app. My params: $*" >> test_app.log
	sleep 20
done
