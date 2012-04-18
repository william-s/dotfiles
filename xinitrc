#!/bin/sh


/home/william/.screenlayout/dual-head.sh

urxvtd -q -f -o
pulseaudio --start &
xcompmgr -c -t-5 -l-5 -r4.2 -o.55 &

if [ -f $HOME/.Xmodmap ]; then
	/usr/bin/xmodmap $HOME/.Xmodmap
fi

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

/usr/bin/dropbox start &
/usr/bin/feh --bg-scale --no-xinerama /home/william/.wallpaper

/usr/bin/xmonad
