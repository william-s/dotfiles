import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName  --for java gui apps
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Spacing
import System.IO

-- Appearence
myFont = "xft:Consolas:pixelsize=16:antialias=true"
colorBlack        = "#060203" --color0
colorBlackAlt     = "#444444" --color8
colorWhite        = "#d9fdee" --color7
colorWhiteAlt      = "#c5e0e1" --color15
colorRed          = "#c54200" --color1     
colorRedAlt       = "#db703a" --color9     
colorYellow       = "#e3a100" --color3    
colorYellowAlt    = "#e5be21" --color11     
colorBlue         = "#3856b8" --color4     
colorBlueAlt      = "#1793d1" --color12     
colorCyan         = "#2e8fac" --color6
colorCyanAlt      = "#48a0b8" --color14

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

myWorkspaces :: [WorkspaceId]
myWorkspaces =
    [ "Term"    
    , "WWW"     
    , "Read"    
    , "Chat"
    , "Misc"
    , "6" , "7" , "8" , "9", "0"
    ]

myXPConfig = defaultXPConfig
    { font                = myFont
    , bgColor             = colorBlack
    , fgColor             = colorWhite
    , bgHLight            = colorBlue
    , fgHLight            = colorWhite
    , borderColor         = colorWhiteAlt
    , promptBorderWidth   = 1
    , height              = 16
    , position            = Top
    , historySize         = 100
    , historyFilter       = deleteConsecutive
    , autoComplete        = Nothing
    }

myStartupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ ewmh defaultConfig
        { terminal    = "urxvtc"
        , modMask     = mod4Mask -- Win key or Super_L
        , borderWidth = 1
        , normalBorderColor = colorBlack
        , focusedBorderColor = colorWhite
        , manageHook = manageDocks <+> myManageHook 
                        <+> manageHook defaultConfig
        , layoutHook = avoidStruts $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppCurrent  = xmobarColor colorRedAlt "" --active tag
                        , ppVisible  = xmobarColor colorYellowAlt "" --visible tag
                        , ppHidden   = xmobarColor colorCyanAlt "" --tag color
                        , ppOutput   = hPutStrLn xmproc
                        , ppTitle    = xmobarColor colorYellow "" . shorten 50
                        }
        , startupHook = myStartupHook
        , workspaces = myWorkspaces
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod1Mask, xK_F2), shellPrompt myXPConfig)
        , ((mod4Mask, xK_F2), xmonadPrompt myXPConfig)
        , ((mod4Mask .|. shiftMask, xK_x), runOrRaisePrompt defaultXPConfig)
        , ((mod4Mask .|. shiftMask, xK_h), spawn "feh --scale ~/Dropbox/reference-cards/Xmbindings.png")
        -- basic CycleWS setup
        , ((mod4Mask,               xK_Down),  nextWS)
        , ((mod4Mask,               xK_Up),    prevWS)
        , ((mod4Mask .|. shiftMask, xK_Down),  shiftToNext)
        , ((mod4Mask .|. shiftMask, xK_Up),    shiftToPrev)
        , ((mod4Mask,               xK_Right), nextScreen)
        , ((mod4Mask,               xK_Left),  prevScreen)
        , ((mod4Mask .|. shiftMask, xK_Right), shiftNextScreen)
        , ((mod4Mask .|. shiftMask, xK_Left),  shiftPrevScreen)
        , ((mod4Mask,               xK_z),     toggleWS)
        ]

