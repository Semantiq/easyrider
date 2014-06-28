#!/bin/sh

APP=$1
VERSION=$2
FILE=$3

curl -v -include --form application=$APP --form version=$VERSION --form content=@$FILE http://localhost:8000/repo/upload

# 1.0-`date +"%T"`

echo "Exit code: $?"
