#!/bin/bash
# Author: William Sherer, modified from
# Original Author: nnoell <nnoell3@gmail.com> http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Nnoell%27s_topstatusbar.sh
# Depends: dzen2-xft-xpm-xinerama-svn && conky
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Layout
BAR_H=13
BIGBAR_W=65
WIDTH_L=870
WIDTH_R=710 #WIDTH_L + WIDTH_R = 1580
HEIGHT=18
X_POS_L=0
X_POS_R=$WIDTH_L
Y_POS=0

#Colors and font
CRIT="#e3a100"
BAR_FG="#1793d1"
BAR_BG="#444444"
DZEN_FG="#d9fdee"
DZEN_FG2="#c5e0e1"  #is default from xmonad.hs
DZEN_BG="#060203"
COLOR_SEP=$DZEN_FG2
FONT="Consolas:pixelsize=16:antialias=true"

#Conky
CONKYFILE="${HOME}/.xmonad/conkyrc"
IFS='|'
INTERVAL=1
CPUTemp=0
GPUTemp=0
CPULoad0=0
CPULoad1=0
MpdInfo=0
MpdRandom="Off"
MpdRepeat="Off"

#clickable areas
VOL_MUTE_CMD="/usr/bin/mute_toggle"
VOL_UP_CMD="/usr/bin/vol_up"
VOL_DOWN_CMD="/usr/bin/vol_up"
DROP_START_CMD="dropbox start"
DROP_STOP_CMD="dropbox stop"
MPD_REP_CMD="mpc -h 127.0.0.1 repeat"
MPD_RAND_CMD="mpc -h 127.0.0.1 random"
MPD_TOGGLE_CMD="ncmpcpp toggle"
MPD_NEXT_CMD="ncmpcpp next"
MPD_PREV_CMD="ncmpcpp prev"
CAL_CMD="sh ${HOME}/bin/dzencal.sh"


printVolInfo() {
    Perc=$(pacmd list-sinks | sed -n 's/volume:\s0:\s*\([0-9]\{1,3\}%\).*/\1/p' | tail -n1)
    Mute=$(pacmd list-sinks | grep "muted: yes")
        echo -n "^fg($DZEN_FG2) ^ca(1,$VOL_MUTE_CMD)^ca(4,$VOL_UP_CMD)^ca(5,$VOL_DOWN_CMD)VOL^ca()^ca()^ca() "
        if [[ $Mute != "" ]]; then
            echo -n "$(echo $Perc | gdbar -fg $CRIT -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
            echo -n "^fg()off"
        else
            echo -n "$(echo $Perc | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
            echo -n "^fg()${Perc}"
            fi
            return
}

printCPUInfo() {
    [[ $CPULoad0 -gt 60 ]] && CPULoad0="^fg($CRIT)$CPULoad0^fg()"
    [[ $CPULoad1 -gt 60 ]] && CPULoad1="^fg($CRIT)$CPULoad1^fg()"
    [[ $CPULoad2 -gt 60 ]] && CPULoad2="^fg($CRIT)$CPULoad2^fg()"
    [[ $CPULoad3 -gt 60 ]] && CPULoad3="^fg($CRIT)$CPULoad3^fg()"
    echo -n " ^fg($DZEN_FG2)CPU ^fg($BAR_FG)${CPULoad0}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad1}%%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad1}%"
    return
}

printTempInfo() {
    CPUTemp=$(sensors | sed -n 's/CPU Temp[^0-9]*\([^(]*\).*/\1/p' | tr -d [:space:])
    #add MB temp? MBTemp=$(sensors | sed -n 's/MB Temp[^0-9]*\([^(]*\).*/\1/p' | tr -d [:space:])
    GPUTemp=$(sensors | grep -A 2 'radeon' | sed -n 's/.*+\(.*$\)/\1/p' | tr -d [:space:])
    [[ $CPUTemp -gt 60 ]] && CPUTemp="^fg($CRIT)$CPUTemp^fg()"
    [[ $GPUTemp -gt 60 ]] && GPUTemp="^fg($CRIT)$GPUTemp^fg()"
    echo -n "^fg($DZEN_FG2)TEMP ^fg($BAR_FG)${CPUTemp}^fg($DZEN_FG2)/^fg($BAR_FG)${GPUTemp}"
    return
}

printMemInfo() {
    [[ $MemPerc -gt 70 ]] # HUH? && CPUTemp="^fg($CRIT)$MemPerc^fg()"
    echo -n "^fg($DZEN_FG2)MEM ^fg($BAR_FG)${MemPerc}%"
    return
}

printDropBoxInfo() {
    DropboxON=$(ps -A | grep -c dropbox)
    if [[ $DropboxON == "0" ]]; then
        echo -n "^fg($DZEN_FG2)^ca(1,$DROP_START_CMD)DROPBOX^ca() ^fg()Off"
    else
        echo -n "^fg($DZEN_FG2)^ca(1,$DROP_STOP_CMD)DROPBOX^ca() ^fg($CRIT)On"
    fi
    return
}

printMpdInfo() {
    MPDON=$(ps -A | grep -c mpd)
        if [[ $MPDON == "0" ]]; then
        echo -n "^fg($DZEN_FG2)^ca(1,mpd)MPD^ca() ^fg()Off"
    else
        [[ $MpdRepeat == "On" ]] && MpdRepeat="^fg($CRIT)$MpdRepeat^fg()"
        [[ $MpdRandom == "On" ]] && MpdRandom="^fg($CRIT)$MpdRandom^fg()"
        echo -n "^fg($DZEN_FG2)^ca(1,$MPD_REP_CMD)REPEAT^ca() ^fg()$MpdRepeat "
        echo -n "^fg($DZEN_FG2)| ^ca(1,$MPD_RAND_CMD)RANDOM^ca() ^fg()$MpdRandom "
        echo -n "^fg($DZEN_FG2)| ^ca(1,$MPD_TOGGLE_CMD)^ca(4,$MPD_NEXT_CMD)^ca(5,$MPD_PREV_CMD)MPD^ca()^ca()^ca() $MpdInfo"
    fi
    return
}

printDateInfo() {
    echo -n "^ca(1,$CAL_CMD)^fg()$(date '+%Y^fg($DZEN_FG2).^fg()%m^fg($DZEN_FG2).^fg()%d^fg($DZEN_FG)/^fg($DZEN_FG2)%a ^fg($DZEN_FG)| ^fg()%H^fg($DZEN_FG2):^fg()%M^fg($DZEN_FG2):^fg()%S')^ca() "
    return
}

printSpace() {
    echo -n " ^fg($COLOR_SEP)|^fg() "
    return
}
printLeft() {
    while true; do
        read CPULoad0 CPULoad1 CPUFreq MemUsed MemPerc MpdInfo MpdRandom MpdRepeat
        printVolInfo
        printSpace
        printDropBoxInfo
        printSpace
        printMpdInfo
        echo -n " ^fg()>^fg($BAR_FG)>^fg($DZEN_FG2)>"
        echo
    done
    return
}
printRight() {
    while true; do
        read CPULoad0 CPULoad1 CPULoad2 CPULoad3 MemUsed MemPerc MpdInfo MpdRandom MpdRepeat
        printCPUInfo
        printSpace
        printMemInfo
        printSpace
        printTempInfo
        printSpace
        printDateInfo
        echo
    done
    return
}
#Print all and pipe into dzen
conky -c $CONKYFILE -u $INTERVAL | printLeft | dzen2 -x $X_POS_L -y $Y_POS -w $WIDTH_L -h $HEIGHT -fn $FONT -ta 'l' -bg $DZEN_BG -fg $DZEN_FG -p -e '' &
conky -c $CONKYFILE -u $INTERVAL | printRight | dzen2 -x $X_POS_R -y $Y_POS -w $WIDTH_R -h $HEIGHT -fn $FONT -ta 'r' -bg $DZEN_BG -fg $DZEN_FG -p -e ''
