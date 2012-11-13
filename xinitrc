#!/bin/sh


$HOME/.screenlayout/dual-head.sh

xrdb -merge $HOME/.Xresources
xrdb -merge $HOME/.colors/custom2

urxvtd -q -f -o
#compton -cCGb -t-5 -l-5 -r4.2 -o.55 
xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &

/usr/bin/xmodmap $HOME/.Xmodmap

xscreensaver &
pulseaudio --start

/usr/bin/dropbox start &
/usr/bin/feh --bg-scale --no-xinerama $HOME/.wallpaper

$HOME/.cabal/bin/xmonad
