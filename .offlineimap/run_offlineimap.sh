#!/bin/bash

read -d '' USAGE << EOF
USAGE:
  -b#: Run a backup with Back in Time (backup job number #, should ideally
       include the maildir ~/.offlineimap.
  -c : String with names of accounts which mails should be counted.
       If omitted all accounts are.
  -q : Run a quick sync.
  
  Requires the MAIL environment variable to be set.
EOF

ACCOUNTS="."
QUICK=no
BACKUP=no
BACKUP_NR=
ERROR=0
while getopts ":b:c:q" opt; do
  case $opt in
    b)
      BACKUP=yes
      BACKUP_NR=$OPTARG;;
    c)
      if [ -n "$OPTARG" ]; then
        ACCOUNTS="$OPTARG"
      fi;;
    q)
      QUICK=yes;;
    \?)
      echo "$USAGE"
      exit 1;;
  esac
done
shift $(($OPTIND - 1))
if [ ! -z "$@" ]; then
  echo "$USAGE"
  exit 1
fi

echo "Starting imap sync."
date

# Check for gnome keyring env, if not try to get it from ~/.Xdbus
if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
  source ~/.Xdbus
fi

# OFFLINEIMAP
read -r pid < ~/.offlineimap/pid

# Exit, if other session already running
if ps $pid &>/dev/null; then
  echo "ERROR: offlineimap ($pid): another instance running." >&2
  exit 1
fi


# Quick sync?
if [ "$QUICK" == "yes" ]
then
  echo "Doing a quick sync."
  offlineimap -o -u quiet -q | grep ERROR && ERROR=1
else
  echo "Doing a full sync."
  offlineimap -o -u quiet | grep ERROR && ERROR=1
fi
# -------------

# COUNT MESSAGES
for ACCOUNT in $(echo $ACCOUNTS)
do
  ACCOUNT=$MAIL"/"$ACCOUNT
  ACC_LIST=$ACC_LIST" "$ACCOUNT
done

echo "Searching for new and unread mails in $ACC_LIST"
# Check, how many new messages there are.
NEW=$(find $ACC_LIST -type f -wholename '*/new/*' | wc -l)
# Check, how many unread messages there are.
UNREAD=$(find $ACC_LIST -type f -regex '.*/cur/.*2,[^S]*$' | wc -l)
if [[ "$NEW" -eq "0" && "$UNREAD" -eq "0" ]]; then
  echo "No new mail."
  rm ~/.offlineimap/.new_mail &> /dev/null
else
  if [[ $ERROR == 0 ]]; then
    echo "$NEW new and $UNREAD unread mails."
    echo $NEW > ~/.offlineimap/.new_mail
    echo $UNREAD >> ~/.offlineimap/.new_mail
  else
    echo "ERROR!"
    echo -e "ERROR\nERROR" > ~/.offlineimap/.new_mail
  fi
fi
# -------------

# MAKE BACKUP
if [ "$BACKUP" == "yes" ]; then
  echo "Doing a backup, backup profile #$BACKUP_NR."
  nice -n 19 ionice -c2 -n7 /usr/bin/backintime --profile-id $BACKUP_NR --backup-job >/dev/null
fi
# -------------

echo
