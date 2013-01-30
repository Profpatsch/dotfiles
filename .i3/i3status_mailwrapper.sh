#!/bin/sh
# shell script to prepend i3status with more stuff

i3status --config ~/.i3/i3status.conf | while :
do
   read line
   NEW_MAIL_FILE=~/.offlineimap/.new_mail
   if [ -f $NEW_MAIL_FILE ]; then
	   # "Mail: 43N/100U | "
	   NEW_MAIL="Mail: N:"$(head -n1 $NEW_MAIL_FILE)" U:"$(tail -n1 $NEW_MAIL_FILE)" | "
   else
	   NEW_MAIL=""
   fi
   echo "$NEW_MAIL$line" || exit 1
done
