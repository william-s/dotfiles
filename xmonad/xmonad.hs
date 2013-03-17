import XMonad
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName  --for java gui apps
import XMonad.Layout.Grid
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.Cursor
import XMonad.Util.Run
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Appearence
myFont = "inconsolata:pixelsize=18:bold:antialias=true"
colorBlack        = "#000000" --color0
colorBlackAlt     = "#545454" --color8
colorWhite        = "#ababab" --color7
colorWhiteAlt     = "#eeeeee" --color15
colorRed          = "#b31919" --color1     
colorRedAlt       = "#ff3333" --color9     
colorYellow       = "#ecb613" --color3    
colorYellowAlt    = "#fafa38" --color11     
colorBlue         = "#7053c6" --color4     
colorBlueAlt      = "#8c8cf2" --color12     
colorCyan         = "#29a385" --color6
colorCyanAlt      = "#52e0e0" --color14

myDzenPP h = defaultPP
            { ppCurrent  = dzenColor colorRedAlt colorBlack . wrap "[" "]" --active tag
            , ppVisible  = dzenColor colorYellowAlt colorBlack . wrap "[" "]"  --visible tag
            , ppHidden   = dzenColor colorCyanAlt   colorBlack  . wrap "" ""  --tag color
            , ppOutput   = hPutStrLn h 
            , ppTitle    = dzenColor colorYellow colorBlack . pad  . shorten 80
            }

keysToAdd x = 
        [ ((mod4Mask .|. shiftMask, xK_z), safeSpawn "xscreensaver-command" ["-lock"])
        , ((controlMask, xK_Print), safeSpawnProg "sleep 0.2; scrot -s")
        , ((0, xK_Print), safeSpawnProg "scrot")
        , ((mod1Mask, xK_F2), shellPrompt myXPconfig)
        , ((mod4Mask, xK_F2), xmonadPrompt myXPconfig)
        , ((mod4Mask .|. shiftMask, xK_x), runOrRaisePrompt defaultXPConfig)
        , ((mod4Mask .|. shiftMask, xK_h), safeSpawn "feh " ["--scale ~/Dropbox/reference-cards/Xmbindings.png"])
        , ((0, xF86XK_AudioRaiseVolume),     safeSpawnProg "/usr/bin/vol_up") 
        , ((0, xF86XK_AudioLowerVolume),     safeSpawnProg "/usr/bin/vol_down")
        , ((0, xF86XK_AudioMute),     safeSpawnProg "/usr/bin/mute_toggle")
        -- launch dmenu
        , ((mod4Mask,               xK_p     ), safeSpawnProg "dmenu_run")
        -- basic CycleWS setup
        , ((mod4Mask,               xK_Down),  nextWS)
        , ((mod4Mask,               xK_Up),    prevWS)
        , ((mod4Mask .|. shiftMask, xK_Down),  shiftToNext)
        , ((mod4Mask .|. shiftMask, xK_Up),    shiftToPrev)
        , ((mod4Mask,               xK_Right), nextScreen)
        , ((mod4Mask,               xK_a),     nextScreen)
        , ((mod4Mask,               xK_Left),  prevScreen)
        , ((mod4Mask .|. shiftMask, xK_Right), shiftNextScreen)
        , ((mod4Mask .|. shiftMask, xK_Left),  shiftPrevScreen)
        , ((mod4Mask,               xK_z),     toggleWS)
        , ((mod4Mask,               xK_q), safeSpawnProg "killall conky dzen2; xmonad --recompile")
        ]

keysToDel x = []

newKeys x = M.union (keys defaultConfig x) (M.fromList (keysToAdd x))
myKeys x = foldr M.delete (newKeys x) (keysToDel x)

myWorkspaceBar :: String
myWorkspaceBar    = "dzen2 -x '0' -y '0' -h '18' -w '1000' -ta 'l' -fg '" ++ colorWhiteAlt ++ "' -bg '" ++ colorBlack ++ "' -fn '" ++ myFont ++ "' -p -e ''"
--myBottomStatusBar = ""
myTopStatusBar    = "/home/william/.xmonad/topbar.sh"

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "MPlayer" --> (ask >>= doF . W.sink)
    , className =? "Chromium"  --> doShift "2"
    , className =? "Firefox"  --> doShift "2"
    ]

myXPconfig = defaultXPConfig
    { font                = myFont
    , bgColor             = colorBlack
    , fgColor             = colorWhite
    , bgHLight            = colorBlue
    , fgHLight            = colorWhite
    , borderColor         = colorWhiteAlt
    , promptBorderWidth   = 1
    , height              = 20
    , position            = Top
    , historySize         = 100
    , historyFilter       = deleteConsecutive
    , autoComplete        = Nothing
    }

myLayoutHook = avoidStruts $  tiled ||| Mirror tiled ||| Full ||| Grid
  where
      -- tiled   = Tall nmaster delta ratio
      tiled   = Tall 1 (3/100) (1/2)

myLogHook = ewmhDesktopsLogHook >> setWMName "LG3D"
myStartupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr

main = do
    workspaceBar <- spawnPipe myWorkspaceBar
    topStatusBar <- spawnPipe myTopStatusBar
    xmonad $ ewmh defaultConfig
        { terminal    = "urxvtc"
        , modMask     = mod4Mask -- Win key or Super_L
        , borderWidth = 1
        , normalBorderColor = colorBlack
        , focusedBorderColor = colorYellow
        , keys = myKeys
        , manageHook = manageDocks <+> myManageHook 
                        <+> manageHook defaultConfig
        , layoutHook = myLayoutHook
        , logHook = myLogHook >> (dynamicLogWithPP $ myDzenPP workspaceBar)
        , startupHook = myStartupHook
        }
