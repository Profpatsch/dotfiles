# This is an attempt to automate the whole system with every file that may be
# only accessible in a special way.

# Everything is relayed to stdout and stderr, no silencing done (do that in
# your cronjob.
echo
echo
date

# MANUAL BACKUPS
# These should be written to ~/.backup.

# crontab
crontab -l > ~/.backup/crontab


# AUTOMATIC BACKUP
backintime --backup-job

