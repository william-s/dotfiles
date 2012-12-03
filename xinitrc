#!/bin/sh

/usr/bin/xcalib -d :0 /usr/share/color/icc/UX31.icc

xrdb -merge $HOME/.Xresources
xrdb -merge $HOME/.colors/custom2

urxvtd -q -f -o
compton -cCGb -t-5 -l-5 -r4.2 -o.55 -i.75
#xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &

/usr/bin/xmodmap $HOME/.Xmodmap

xscreensaver &

/usr/bin/dropbox start &

/usr/bin/feh --bg-center $HOME/.wallpaper
$HOME/.cabal/bin/xmonad
