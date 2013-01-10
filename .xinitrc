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

# set mouse speeds      device  threshold  accel  deccel
# touchpad
xinput set-ptr-feedback 13 	18 	   13 	  5

# Autostart
# music playing daemon
mpd
# mpd scrobbler
mpdscribble
# dropbox
dropboxd &
# skype
skype &

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
# exec xterm
# exec enlightenment_start
exec i3 -V >> ~/.i3/i3log 2>&1 #i3 with logging
