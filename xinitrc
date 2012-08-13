#!/bin/sh

/usr/bin/xcalib -d :0 /usr/share/color/icc/UX31.icc

urxvtd -q -f -o
#compton -cCGb -t-5 -l-5 -r4.2 -o.45 
xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &

/usr/bin/xmodmap $HOME/.Xmodmap

xrdb -merge $HOME/.Xresources

xscreensaver &

/usr/bin/dropbox start &
/usr/bin/feh --bg-center /home/william/.wallpaper

ck-launch-session xmonad
#ck-launch-session startlxde
