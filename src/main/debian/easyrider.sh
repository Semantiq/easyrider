#!/bin/bash

### BEGIN INIT INFO
# Provides:          easyrider
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start EasyRider at boot time
# Description:       Run EasyRider deamon
### END INIT INFO

COMMAND="$1"

start() {
	echo -ne "Starting EasyRider ...\t"
	start-stop-daemon --exec /usr/bin/easyrider -c www-data -b -m -p /var/tmp/easyrider.pid -S -- $PROPS -mem 64 -Dconfiguration=/etc/easyrider/configuration.json -Dworking.directory=/opt/easyrider
}

stop() {
	echo -ne "Stopping EasyRider ...\t"
	start-stop-daemon -K -p /var/tmp/easyrider.pid
}

status() {
	start-stop-daemon -T -p /var/tmp/easyrider.pid
	STATUS=$?
	echo "Status $STATUS"
	exit $STATUS
}

case $COMMAND in
start)
	start
	status
	;;
stop)
	stop
	status
	;;
restart)
	stop
	status
	start
	status
	;;
status)
	status
	;;
esac
