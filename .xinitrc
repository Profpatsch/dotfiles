!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# set hold key delay
xset r rate 250 35

# execute program for shortcut assigning (runs as daemon)
xbindkeys

# set general mouse speed
# Accel: 8/3 (2,666) Threshold: ?
xset m 8/3
# set mouse speeds      device  threshold  accel  deccel
# touchpad
#xinput set-ptr-feedback 13 	18 	   13 	  5

# Start a D-Bus session
source /etc/X11/xinit/xinitrc.d/30-dbus
# Start GNOME Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# You probably need to do this too:
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_CONTROL
export GNOME_KEYRING_PID
# Export the dbus session address on startup so it can be used by cron
touch ~/.Xdbus
chmod 600 ~/.Xdbus
env | grep DBUS_SESSION_BUS_ADDRESS > ~/.Xdbus
echo 'export DBUS_SESSION_BUS_ADDRESS' >> ~/.Xdbus 
  
  # Autostart
  # file manager daemon
  #thunar --daemon & # No significant speed-up
  # music playing daemon
  mpd
  # mpd scrobbler
  mpdscribble
  # dropbox
  dropboxd &
  # skype
  skype &
  # pidgin
pidgin &

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
# exec xterm
# exec enlightenment_start
exec i3 -V >> ~/.logs/i3.log 2>&1 #i3 with logging
