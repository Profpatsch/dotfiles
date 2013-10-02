#!/bin/sh
# shell script to prepend i3status with more stuff

i3status --config .i3/i3status_other_monitors.conf | while :
do
       read line
       echo "applepie@soarin | $line" || exit 1
done
