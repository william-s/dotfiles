#!/bin/sh


/home/william/.screenlayout/dual-head.sh

urxvtd -q -f -o
xcompmgr -c -t-5 -l-5 -r4.2 -o.55 &

/usr/bin/xmodmap $HOME/.Xmodmap

xrdb -merge $HOME/.Xresources

xscreensaver &

/usr/bin/dropbox start &
/usr/bin/feh --bg-scale --no-xinerama /home/william/.wallpaper

/home/william/bin/xmonad
