#!/bin/bash

FILE=test_app/test_app.zip
VERSION=`date +'%Y%m%d'`-`date +"%T"`

while getopts a:v:f: option; do
	case "${option}" in
		f) FILE=${OPTARG};;
		a) APP=${OPTARG};;
		v) VERSION=${OPTARG};;
	esac
done

echo "Uploading $APP v $VERSION from $FILE"

curl -v -include --user test:test --form application=$APP --form version=$VERSION --form content=@$FILE http://localhost:8000/repo/upload


echo "Exit code: $?"
