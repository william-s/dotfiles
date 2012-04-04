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
import XMonad.Util.Run(spawnPipe)
import System.IO


import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Appearence
font' = "inconsolata:pixelsize=16:bold:antialias=true"
colorBlack        = "#060203" --color0
colorBlackAlt     = "#444444" --color8
colorWhite        = "#d9fdee" --color7
colorWhiteAlt     = "#c5e0e1" --color15
colorRed          = "#c54200" --color1     
colorRedAlt       = "#db703a" --color9     
colorYellow       = "#e3a100" --color3    
colorYellowAlt    = "#e5be21" --color11     
colorBlue         = "#3856b8" --color4     
colorBlueAlt      = "#1793d1" --color12     
colorCyan         = "#2e8fac" --color6
colorCyanAlt      = "#48a0b8" --color14

dzenPP' h = defaultPP
            { ppCurrent  = dzenColor colorRedAlt colorBlack . wrap "[" "]" --active tag
            , ppVisible  = dzenColor colorYellowAlt colorBlack . wrap "[" "]"  --visible tag
            , ppHidden   = dzenColor colorCyanAlt   colorBlack  . wrap "" ""  --tag color
            , ppOutput   = hPutStrLn h 
            , ppTitle    = dzenColor colorYellow colorBlack . pad  . shorten 80
            }

keysToAdd x = 
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod1Mask, xK_F2), shellPrompt myXPconfig)
        , ((mod4Mask, xK_F2), xmonadPrompt myXPconfig)
        , ((mod4Mask .|. shiftMask, xK_x), runOrRaisePrompt defaultXPConfig)
        , ((mod4Mask .|. shiftMask, xK_h), spawn "feh --scale ~/Dropbox/reference-cards/Xmbindings.png")
        , ((0, xF86XK_AudioRaiseVolume),     spawn "/usr/bin/vol_up") --raise sound
        , ((0, xF86XK_AudioLowerVolume),     spawn "/usr/bin/vol_down") --lower sound
        , ((0, xF86XK_AudioMute),     spawn "/usr/bin/mute_toggle") --mute sound
        -- launch dmenu
        , ((mod4Mask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
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
        ]

keysToDel x = []

newKeys x = M.union (keys defaultConfig x) (M.fromList (keysToAdd x))
keys' x = foldr M.delete (newKeys x) (keysToDel x)

workspaceBar', bottomStatusBar', topStatusBar' :: String
workspaceBar'    = "dzen2 -x '1050' -y '0' -h '18' -w '1000' -ta 'l' -fg '" ++ colorWhiteAlt ++ "' -bg '" ++ colorBlack ++ "' -fn '" ++ font' ++ "' -p -e ''"
bottomStatusBar' = ""
topStatusBar'    = "/home/william/.xmonad/topbar.sh"

manageHook' = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    , className =? "Chromium"  --> doShift "WWW" 
    ]

workspaces' :: [WorkspaceId]
workspaces' =
    [ "Term"    
    , "WWW"     
    , "Chat"    
    , "Read"
    , "Misc"
    , "6" , "7" , "8" , "9", "0"
    ]

myXPconfig = defaultXPConfig
    { font                = font'
    , bgColor             = colorBlack
    , fgColor             = colorWhite
    , bgHLight            = colorBlue
    , fgHLight            = colorWhite
    , borderColor         = colorWhiteAlt
    , promptBorderWidth   = 1
    , height              = 18
    , position            = Top
    , historySize         = 100
    , historyFilter       = deleteConsecutive
    , autoComplete        = Nothing
    }

layoutHook' = avoidStruts $  tiled ||| Mirror tiled ||| Full ||| Grid
  where
      -- default tiling algorithm partitions the screen into two panes
      tiled   = Tall nmaster delta ratio
      -- The default number of windows in the master pane
      nmaster = 1
      -- Default proportion of screen occupied by master pane
      ratio   = 1/2
      -- Percent of screen to increment by when resizing panes
      delta   = 3/100

logHook' = ewmhDesktopsLogHook >> setWMName "LG3D"
startupHook' = setWMName "LG3D" >> setDefaultCursor xC_left_ptr

main = do
    workspaceBar <- spawnPipe workspaceBar'
    topStatusBar <- spawnPipe topStatusBar'
    xmonad $ ewmh defaultConfig
        { terminal    = "urxvtc"
        , modMask     = mod4Mask -- Win key or Super_L
        , borderWidth = 1
        , normalBorderColor = colorBlack
        , focusedBorderColor = colorWhite
        , keys = keys'
        , manageHook = manageDocks <+> manageHook' 
                        <+> manageHook defaultConfig
        , layoutHook = layoutHook'
        , logHook = logHook' >> (dynamicLogWithPP $ dzenPP' workspaceBar)
        , startupHook = startupHook'
        , workspaces = workspaces'
        }
