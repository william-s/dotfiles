#!/bin/bash
# Author: William Sherer, modified from
# Original Author: nnoell <nnoell3@gmail.com> http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Nnoell%27s_topstatusbar.sh
# Depends: dzen2-xft-xpm-xinerama-svn && conky
# Desc: dzen2 bar for XMonad, ran within xmonad.hs via spawnPipe

#Layout
BAR_H=10
BIGBAR_W=65
WIDTH_L=400
WIDTH_R=720 #WIDTH_L + WIDTH_R = 1120
HEIGHT=16
X_POS_L=2480
X_POS_R=2880
Y_POS=0

#Colors and font
CRIT="#e3a100"
BAR_FG="#1793d1"
BAR_BG="#444444"
DZEN_FG="#c5e0e1"
DZEN_FG2="#48a0b8"  #is default from xmonad.hs
DZEN_BG="#060203"
COLOR_SEP="#3856b8"
SEP=" "
FONT="Consolas:pixelsize=16:antialias=true"

#Conky
CONKYFILE="${HOME}/.xmonad/conkyrc"
IFS='|'
INTERVAL=1
DateTime=""
CPUTemp=0
MBDTemp=0
GPUTemp=0
CPULoad0=0
CPULoad1=0
CPULoad2=0
CPULoad3=0
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
    Perc=$(pacmd list-sinks | sed -n 's/\svolume:\s0:\s*\([0-9]\{1,3\}%\).*/\1/p' | tail -n1)
    Mute=$(pacmd list-sinks | grep "muted: yes")
        echo -n "^fg() ^ca(1,$VOL_MUTE_CMD)^ca(4,$VOL_UP_CMD)^ca(5,$VOL_DOWN_CMD)VOL^ca()^ca()^ca() "
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
    echo -n " ^fg()CPU ^fg($BAR_FG)${CPULoad0}%^fg()/^fg($BAR_FG)${CPULoad1}%^fg()/^fg($BAR_FG)${CPULoad2}%^fg()/^fg($BAR_FG)${CPULoad3}%"
    return
}

printTempInfo() {
    #CPUTemp=$(sensors | sed -n 's/CPU Temp[^0-9]*\([0-9]\+\).*/\1/p')
    #MBDTemp=$(sensors | sed -n 's/MB Temp[^0-9]*\([0-9]\+\).*/\1/p')
    #GPUTemp=$(sensors | sed -n 's/temp1.*+\([0-9]\+\).*/\1/p')
    [[ $CPUTemp -gt 60 ]] && CPUTemp="^fg($CRIT)$CPUTemp^fg()"
    [[ $MBDTemp -gt 65 ]] && MBDTemp="^fg($CRIT)$MBDTemp^fg()"
    [[ $GPUTemp -gt 65 ]] && GPUTemp="^fg($CRIT)$GPUTemp^fg()"
    echo -n "^fg()TEMP ^fg($BAR_FG)${CPUTemp}°^fg()/^fg($BAR_FG)${MBDTemp}°^fg()/^fg($BAR_FG)${GPUTemp}°"
    return
}

printMemInfo() {
    [[ $MemPerc -gt 70 ]] && $MemPerc="^fg($CRIT)$MemPerc^fg()"
    echo -n "^fg()MEM ^fg($BAR_FG)${MemPerc}%"
    return
}

printDropBoxInfo() {
    DropboxON=$(ps -A | grep -c dropbox)
    if [[ $DropboxON == "0" ]]; then
        echo -n "^fg()^ca(1,$DROP_START_CMD)DROPBOX^ca() ^fg()Off"
    else
        echo -n "^fg()^ca(1,$DROP_STOP_CMD)DROPBOX^ca() ^fg($CRIT)On"
    fi
    return
}

printMpdInfo() {
    MPDON=$(ps -A | grep -c mpd)
        if [[ $MPDON == "0" ]]; then
        echo -n "^fg()^ca(1,mpd)MPD^ca() ^fg()Off"
    else
        [[ $MpdRepeat == "On" ]] && MpdRepeat="^fg($CRIT)$MpdRepeat^fg()"
        [[ $MpdRandom == "On" ]] && MpdRandom="^fg($CRIT)$MpdRandom^fg()"
        echo -n "^fg()^ca(1,$MPD_REP_CMD)REPEAT^ca() ^fg()$MpdRepeat "
        echo -n "^fg()^$SEP^ca(1,$MPD_RAND_CMD)RANDOM^ca() ^fg()$MpdRandom "
        echo -n "^fg()^$SEP^ca(1,$MPD_TOGGLE_CMD)^ca(4,$MPD_NEXT_CMD)^ca(5,$MPD_PREV_CMD)MPD^ca()^ca()^ca() $MpdInfo"
    fi
    return
}

printDateInfo() {
    #echo -n "^ca(1,$CAL_CMD)^fg()$(date '+%Y^fg($COLOR_SEP).^fg()%m^fg($COLOR_SEP).^fg()%d/^fg($DZEN_FG2)%a ^fg($COLOR_SEP)^$SEP^fg()%H^fg($COLOR_SEP):^fg()%M^fg($COLOR_SEP):^fg()%S')^ca() "
    echo -n "^ca(1,$CAL_CMD)^fg()$DateTime^ca()" 
    return
}

printSpace() {
    echo -n "^fg($COLOR_SEP)$SEP^fg()"
    return
}
printLeft() {
    while true; do
        read DateTime CPULoad0 CPULoad1 CPULoad2 CPULoad3 MemPerc FSroot FShome CPUTemp MBDTemp GPUTemp
        printVolInfo
        printSpace
        printDropBoxInfo
        printSpace
        printMpdInfo
        echo -n " ^fg()>^fg($BAR_FG)>^fg()>"
        echo
    done
    return
}
printRight() {
    while true; do
        read DateTime CPULoad0 CPULoad1 CPULoad2 CPULoad3 MemPerc FSroot FShome CPUTemp MBDTemp GPUTemp
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
