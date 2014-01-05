#!/bin/sh

echo "Ok!" > $FILE

if [ "$CRASH" != "true" ]; then
    while [ "1" == "1" ]; do
        sleep 1
        touch $FILE
    done
fi
