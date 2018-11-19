import XMonad
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Util.EZConfig
import XMonad.ManageHook
import XMonad.Hooks.UrgencyHook

import XMonad.Hooks.SetWMName --import XMonad for java
import XMonad.Layout.Spacing -- for space beetwean a tile

import System.IO

import qualified Data.Map as M
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import qualified XMonad.Util.Brightness as Bright


myXmonadBar = "dzen2 -x '980' -y '0' -h '24' -w '940' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
myStatusBar = "conky -c /home/my_user/.xmonad/.conky_dzen | dzen2 -x '2080' -w '1040' -h '24' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -y '0'"

main = do
   dzenLeftBar <- spawnPipe myXmonadBar
   dzenRightBar <- spawnPipe myStatusBar
   -- xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
   xmonad $ defaultConfig
     { terminal    = myTerminal
     , modMask     = myModMask
     , borderWidth = myBorderWidth
     , workspaces  = myWorkspace
     , manageHook  = manageDocks <+> manageHook defaultConfig
     , layoutHook  = spacing 17 $ layoutHook defaultConfig  --Tall 1 (3/100) (1/2) -- windows spacign on 15 piksels
     , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
     , startupHook = setWMName "LG3D"
     , normalBorderColor        = "#001230" -- "#001122" --"#222222" -- RGBA
     , focusedBorderColor       = "#00FFFF" --"#00FFFF"
     , logHook     = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xeeeeeeee--0xdddddddd
     } `additionalKeys` myKeys


myTerminal    = "/usr/bin/gnome-terminal"
myModMask     = mod4Mask -- Alt (Super_L)
myBorderWidth = 0
myWorkspace :: [String]
myWorkspace = ["1:web", "2:dev", "3:media", "4:msg"] ++ map show [5..9]
myManagehook :: ManageHook
myManagehook = composeAll . concat $
  [
    [className =? c --> doShift "1:web"            | c <- myWeb],
    [className =? c --> doShift "3:media"          | c <- myMedia],
    [className =? c --> doShift "4:msg"            | c <- myMessg],
    [manageDocks]
  ]
  where
    myWeb       = ["firefox", "vivaldi", "opera"]
    myMedia     = ["zathura", "evince", "rhytmbox"]
    myMessg     = ["viber", "Discord"]


myKeys =
  [
    ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock"),
    ((0                     , 0x1008FF11), spawn "amixer -q sset Master 5%-"),
    ((0                     , 0x1008FF13), spawn "amixer -q sset Master 5%+"),
    ((0                     , 0x1008FF12), spawn "amixer set Master toggle")
  ]
  ++
  [
    ((0, 0xFF61), spawn "scrot -q 1 $HOME/pictures/screenshots/%Y-%m-%d-%H:%M:%S.png"), 
    ((mod4Mask .|. shiftMask, xK_n), spawn "nautilus -w"),
    ((mod4Mask .|. shiftMask, xK_t), spawn "thunar"),
    ((mod4Mask .|. shiftMask, xK_o), spawn "opera"),
    ((mod4Mask .|. shiftMask, xK_v), spawn "vivaldi"),
    ((mod4Mask .|. shiftMask, xK_r), spawn "rhythmbox"),
    ((mod4Mask .|. shiftMask, xK_s), spawn "steam"),
    ((mod4Mask .|. shiftMask, xK_p), spawn "gnome-screenshot -a")
  ]
  ++
  [
    ((mod4Mask, xK_Escape), spawn "/home/serhii/.bin/layout_kb_switcher.sh")
  ]
  ++
  [
    ((0, XF86.xF86XK_MonBrightnessUp), Bright.increase),       -- Increase screen brightness SUPER + 8/9
    ((0, XF86.xF86XK_MonBrightnessDown), Bright.decrease)
  ]
  ++
  [
    ((0, XF86.xF86XK_AudioNext), spawn "mocp -f"),
    ((0, XF86.xF86XK_AudioPrev), spawn "mocp -r"),
    ((0, XF86.xF86XK_AudioStop), spawn "mocp -P"),
    ((0, XF86.xF86XK_AudioPlay), spawn "mocp -P")
  ]

myBitmapsDir = "/home/my_user/.xmonad/dzen2"
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#00FFFF" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#00FFFF" "#1B1D1E" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
