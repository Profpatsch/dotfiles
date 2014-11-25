#!/bin/sh
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

# Disable internal speaker beeps:
xset -b

# set hold key delay
xset r rate 250 35

setxkbmap de neo
# swap Super (Win) and Alt
setxkbmap -option altwin:swap_alt_win

# execute program for shortcut assigning (runs as daemon)
xbindkeys

# set general mouse speed
# Accel: 8/3 (2,666) Threshold: ?
xset m 8/3
# set mouse speeds      device  threshold  accel  deccel
# touchpad
#xinput set-ptr-feedback 13 	18 	   13 	  5

# Set right resolution (b/c of UXA-mode in /etc/X11/xorg.conf.d/30-intel.conf)
xrandr --output LVDS1 --mode 1440x900
xrandr --output VGA1 --mode 1280x1024

# Set X program settings
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

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

# Automounting (gvfs, udiskie, udev)
udiskie -2 &
  

# Autostart
# pidgin chat
pidgin &
# redshift (auto color temperature for night.)
toggle-redshift &
# notifications
dunst &
# Network manager
nm-applet &
# News reader
liferea &
# Dropbox
dropboxd &


# Background
set-background

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice
# exec xterm
# exec enlightenment_start

# Put some environment variables in the config
~/.i3/config-config
exec i3 -c ~/.i3/config_catted >> ~/.logs/i3.log #i3 with logging
