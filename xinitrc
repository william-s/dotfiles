#!/bin/sh

/usr/bin/xcalib -d :0 /usr/share/color/icc/UX31.icc

xrdb -merge $HOME/.Xresources
xrdb -merge $HOME/.colors/custom2

urxvtd -q -f -o
compton -bcCGf -t-5 -l-5 -r4 -o.65 -i.85
#xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &

/usr/bin/xmodmap $HOME/.Xmodmap

xscreensaver &
pulseaudio --start

/usr/bin/dropbox start &

/usr/bin/feh --bg-center $HOME/.wallpaper
$HOME/.cabal/bin/xmonad
