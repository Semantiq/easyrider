#!/bin/sh
#TODO: move to SBT
BASEDIR=`pwd`

(
    cd $BASEDIR/core/target/universal/
    rm -r */
    unzip *.zip
    cd */
    cp $BASEDIR/core/src/main/bin/init .
    tar -c * | gzip > $BASEDIR/core/target/easyrier.tar.gz
)
