#!/bin/bash

read -d '' USAGE << EOF
USAGE:
  -b#: Run a backup with Back in Time (backup job number #, should ideally
       include the maildir ~/.offlineimap.
  -c : String with names of accounts which mails should be counted.
       If omitted all accounts are.
  -q : Run a quick sync.
EOF

ACCOUNTS="."
QUICK=no
BACKUP=no
BACKUP_NR=
while getopts ":b:c:q" opt; do
  case $opt in
    b)
      BACKUP=yes
      BACKUP_NR=$OPTARG;;
    c)
      if [ ! -z $OPTARG ]; then
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

# OFFLINEIMAP
read -r pid < ~/.offlineimap/pid

# Kill old session, if still running
if ps $pid &>/dev/null; then
  echo "offlineimap ($pid): another instance running." >&2
  kill -9 $pid
fi

# Quick sync?
if [ "$QUICK" == "yes" ]
then
  offlineimap -o -u quiet -q
else
  offlineimap -o -u quiet
fi
# -------------

# COUNT MESSAGES
for ACCOUNT in $(echo $ACCOUNTS)
do
  ACCOUNT=$MAIL"/"$ACCOUNT
  ACC_LIST=$ACC_LIST" "$ACCOUNT
done

# Check, how many new messages there are.
NEW=$(find $ACC_LIST -type f -wholename '*/new/*' | wc -l)
# Check, how many unread messages there are.
UNREAD=$(find $ACC_LIST -type f -regex '.*/cur/.*2,[^S]*$' | wc -l)
if [ "$NEW" -eq "0" ]; then
  if [ "$UNREAD" -eq "0" ]; then
    rm ~/.offlineimap/.new_mail &> /dev/null
  fi
else
  echo $NEW > ~/.offlineimap/.new_mail
  echo $UNREAD >> ~/.offlineimap/.new_mail
fi
# -------------

# MAKE BACKUP
if [ "$BACKUP" == "yes" ]; then
  nice -n 19 ionice -c2 -n7 /usr/bin/backintime --profile-id $BACKUP_NR --backup-job >/dev/null 2>&1
fi
# -------------
