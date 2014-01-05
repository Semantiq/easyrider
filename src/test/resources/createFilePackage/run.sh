#!/bin/sh

echo "Ok!" > $FILE

if [ "$CRASH" != "true" ]; then
    while [ "1" == "1" ]; do
        sleep 10
    done
fi
