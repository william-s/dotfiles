#!/bin/sh


/home/william/.screenlayout/dual-head.sh

xrdb -merge $HOME/.Xresources

urxvtd -q -f -o
#compton -cCGb -t-5 -l-5 -r4.2 -o.55 
xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &

/usr/bin/xmodmap $HOME/.Xmodmap

xscreensaver &
pulseaudio --start

/usr/bin/dropbox start &
/usr/bin/feh --bg-scale --no-xinerama /home/william/.wallpaper

ck-launch-session /home/william/.cabal/bin/xmonad
