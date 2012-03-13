import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName  --for java gui apps
import XMonad.Util.Cursor
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Spacing
import System.IO

myManageHook = composeAll
    [ className =? "Gimp"      --> doFloat
    , className =? "Vncviewer" --> doFloat
    ]

myStartupHook = setWMName "LG3D" >> setDefaultCursor xC_left_ptr

main = do
    xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
        { terminal    = "urxvt"
        , modMask     = mod4Mask -- Win key or Super_L
        , borderWidth = 1
        , normalBorderColor = "#060203"
        , focusedBorderColor = "#c5e0e1"
        , manageHook = manageDocks <+> myManageHook 
                        <+> manageHook defaultConfig
        , layoutHook = avoidStruts $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#38b856" "" . shorten 50
                        }
        , startupHook = myStartupHook
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]

