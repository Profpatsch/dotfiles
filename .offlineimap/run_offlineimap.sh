#!/bin/bash

ACCOUNTS="Privat Mail"

read -r pid < ~/.offlineimap/pid
rm ~/.offlineimap/.new_mail #> /dev/null

# Kill old session, if still running
if ps $pid &>/dev/null; then
  echo "offlineimap ($pid): another instance running." >&2
  kill -9 $pid
fi

offlineimap -o -u quiet

for ACCOUNT in $(echo $ACCOUNTS)
do
  ACCOUNT=$MAIL"/"$ACCOUNT
  ACC_LIST=$ACC_LIST" "$ACCOUNT
done

# Check, how many new messages there are.
NEW=$(find $ACC_LIST -type f -wholename '*/new/*' | wc -l)
# Check, how many unread messages there are.
UNREAD=$(find $ACC_LIST -type f -regex '.*/cur/.*2,[^S]*$' | wc -l)
echo $NEW > ~/.offlineimap/.new_mail
echo $UNREAD >> ~/.offlineimap/.new_mail
