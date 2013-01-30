#!/bin/bash
read -r pid < ~/.offlineimap/pid
rm ~/.offlineimap/.new_mail #> /dev/null

# Kill old session, if still running
if ps $pid &>/dev/null; then
  echo "offlineimap ($pid): another instance running." >&2
  kill -9 $pid
fi

offlineimap -o -u quiet

# Check, how many new messages there are.
NEW=$(find $MAIL -type f -wholename '*/new/*' | wc -l)
# Check, how many unread messages there are.
UNREAD=$(find $MAIL -type f -regex '.*/cur/.*2,[^S]*$' | wc -l)
echo "N: "$NEW" | U: "$UNREAD
echo $NEW > ~/.offlineimap/.new_mail
echo $UNREAD >> ~/.offlineimap/.new_mail
