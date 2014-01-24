#!/bin/bash

### BEGIN INIT INFO
# Provides:          easyrider
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Start EasyRider at boot time
# Description:       Run EasyRider deamon
### END INIT INFO

COMMAND="$1"

# To enable JMX monitoring
if [ -r /etc/easyrider/jmx.conf ]; then
    PROPS="-Dcom.sun.management.jmxremote.port=8200 -Dcom.sun.management.jmxremote.password.file=/etc/easyrider/jmx.conf -Dcom.sun.management.jmxremote.ssl=false -Dcom.sun.management.jmxremote.access.file=/etc/easyrider/jmx.access"
fi

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
