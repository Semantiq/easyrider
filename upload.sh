#!/bin/sh

APP=$1
VERSION=$2
FILE=$3

curl -v -include --form application=$APP --form version=$VERSION --form upload=@$FILE http://localhost:8000/repo/upload

echo "Exit code: $?"
