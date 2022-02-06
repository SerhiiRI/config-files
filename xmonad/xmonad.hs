-- -*- mode: haskell; mode: rainbow; -*-
-- URL for keys: https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Util-EZConfig.html
-- Base
import XMonad hiding((|||))
import System.Directory
import System.IO
import System.Exit (exitSuccess)
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import qualified XMonad.Actions.GridSelect as GS
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.SpawnOn (spawnAndDo)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M
    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat, isDialog)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout(JumpToLayout))
-- import XMonad.Layout.LayoutCombinators -- (JumpToLayout(JumpToLayout))
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import qualified XMonad.Util.Loggers as Logr
import qualified XMonad.Util.Brightness as Bright


   -- Constants --
draculaBackground   = 0xee282a36  -- #282a36
draculaSelection    = 0xff44475a  -- #44475a
draculaForeground   = 0xfff8f8f2  -- #f8f8f2
draculaComment      = 0xff6272a4  -- #6272a4
draculaCyan         = 0xff8be9fd  -- #8be9fd
draculaGreen        = 0xff50fa7b  -- #50fa7b
draculaOrange       = 0xffffb86c  -- #ffb86c
draculaPink         = 0xffff79c6  -- #ff79c6
draculaPurple       = 0xffbd93f9  -- #bd93f9
draculaRed          = 0xffff5555  -- #ff5555
draculaYellow       = 0xfff1fa8c  -- #f1fa8c

sDracBackground   = "#282a36"  -- #282a36
sDracSelection    = "#44475a"  -- #44475a
sDracForeground   = "#f8f8f2"  -- #f8f8f2
sDracComment      = "#6272a4"  -- #6272a4
sDracCyan         = "#8be9fd"  -- #8be9fd
sDracGreen        = "#50fa7b"  -- #50fa7b
sDracOrange       = "#ffb86c"  -- #ffb86c
sDracPink         = "#ff79c6"  -- #ff79c6
sDracPurple       = "#bd93f9"  -- #bd93f9
sDracRed          = "#ff5555"  -- #ff5555
sDracYellow       = "#f1fa8c"  -- #f1fa8c

color01="#000000"
color02=sDracRed
color03=sDracGreen
color04=sDracYellow
color05=sDracPurple
color06=sDracPink
color07=sDracCyan
color08="#bfbfbf"
color09="#4d4d4d"
color10="#ff6e67"
color11="#5af78e"
color12="#f4f99d"
color13="#caa9fa"
color14="#ff92d0"
color15="#9aedfe"
color16="#e6e6e6"


   -- Status Bar --
myXmonadBar = "dzen2 -dock -y '0' -x '0' -w '1920' -h '23' -ta 'l' -fg '#9c7cbf' -bg '#000000' -fn 'Open Sans:style=Bold' "
ovpnFreshcode = "sudo openvpn --config $HOME/fcode/docs/client.ovpn --auth-user-pass $HOME/fcode/docs/pass.txt"
dmenu                   = "dmenu_run -nb '#000000' -nf '#F9F9F9' -sb '#87428a' -sf '#FFFFFF'"
dmenufm                 = "dmenufm"
dmenuKillProcess        = "dmenu_kill_process"
dmenuShootUp            = "dmenu_shoot_app"
dmenuEditConfig         = "dmenu_edit_config"
dmenuMachineControl     = "dmenu_machine_control"
dmenuSoundControl       = "dmenu_sound_control"
dmenuNetworkManager     = "networkmanager_dmenu"
myEditor                = "emacsclient -c -a emacs "
myTerminal              = "urxvt"
myModMask               = mod4Mask
-- Workspace configuration
myWorkspace :: [String] 
myWorkspace             = ["dev a", "dev b", "web", "other", "media", "messages"] ++ (map show [7..8]) ++ ["output"]
-- Configuration of window spacing 
-- border screen size for all monitor;
brdScreenSize :: Integer
brdScreenSize = 0
-- border for each window in screen;
brdWindowSize :: Integer
brdWindowSize = 13
-- defing self border set;
myScreenBorder = Border brdScreenSize brdScreenSize brdScreenSize brdScreenSize
myWindowBorder = Border brdWindowSize brdWindowSize brdWindowSize brdWindowSize
-- creating function for layout modifier
mySpacing :: (l a -> ModifiedLayout Spacing l a)
mySpacing = spacingRaw True myScreenBorder True myWindowBorder True
-- Terminal function prompt
-- myTermPrompt "Resource Monitor" "htop"
myTermPrompt :: String -> String -> String
myTermPrompt title command = "urxvt -title \""++ title ++"\" -e sh -c "++ command

main = do
   -- dzenLeftBar <- spawnPipe myXmonadBar
   xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.xmonad/xmobarrc")
   xmonad $ def
     { terminal        = myTerminal
     , modMask         = myModMask
     , workspaces      = myWorkspace
     , layoutHook      = showWName' myShowWNameTheme $ myLayoutHook
     , manageHook      = myManageHook
     , handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook
     , startupHook     = myStartupHook
     -- Borders
     , borderWidth         = 3
     , normalBorderColor   = "#282A36"
     , focusedBorderColor  = "#9c7cbf"
     -- , logHook         = myDzenLogHook dzenLeftBar >>  fadeWindowsLogHook myFadeHook
     , logHook         = myXmobarLogHook xmproc0 >> fadeWindowsLogHook myFadeHook
     } `additionalKeysP` myKeys

-- show floated hint window, which inform about
-- active selected workspace
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
                   { swn_font      = "xft:Ubuntu Mono:bold:size=60"
                   , swn_fade      = 0.3
                   , swn_bgcolor   = "#282a36"
                   , swn_color     = "#ffffff"
                   }

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "/usr/bin/emacs --daemon &"
  spawnOnce "picom --config ~/.config/xdg/picom.conf &"
  setWMName "LG3D"

myFadeHook = composeAll
    [ opaque -- default to opaque
    , isUnfocused --> opacity 0.93
    , (className =? "Vivaldi-stable") <&&> (isUnfocused) --> opacity 0.93
    , (className =? "Totem") <&&> (isUnfocused) --> opacity 1.0
    , (className =? "Eog") <&&> (isUnfocused) --> opacity 1.0
    , (title =? "Picture in picture") <&&> (isUnfocused) --> opacity 1.0
    -- , (className =? "URxvt") <&&> (isUnfocused) --> opacity 1.0
    , fmap ("Google" `S.isPrefixOf`) className --> opaque
    , isDialog --> opaque
    --, isUnfocused --> opacity 0.55
    --, isFloating  --> opacity 0.75
    ]

-- myManagehook :: ManageHook
-- myManagehook = composeAll . concat $
--   [
--     [className =? c --> doShift "web"            | c <- myWeb],
--     [className =? c --> doShift "media"          | c <- myMedia],
--     [className =? c --> doShift "msg"            | c <- myMessg],
--     [manageDocks]
--   ]
--   where
--     myWeb       = ["firefox", "vivaldi", "opera"]
--     myMedia     = ["zathura", "evince", "rhytmbox"]
--     myMessg     = []

myManageHook :: ManageHook
myManageHook = composeAll 
  [ className =? "confirm"            --> doFloat
  , className =? "file_progress"      --> doFloat
  , className =? "dialog"             --> doFloat
  , className =? "download"           --> doFloat
  , className =? "error"              --> doFloat
  , className =? "notification"       --> doFloat
  , className =? "splash"             --> doFloat
  , className =? "toolbar"            --> doFloat
  , className =? "Eog"                --> doFloat
  , className =? "Gthumb"             --> doFloat
  , title     =? "Jarman"             --> doFloat
  , title     =? "Picture in picture" --> doFloat
  , className =? "Gnome-screenshot"     --> doCenterFloat
  , className =? "Org.gnome.Nautilus"   --> doCenterFloat
  , className =? "SimpleScreenRecorder" --> doCenterFloat
  ] <+> manageDocks
    

--------------------
--- Layout Hooks ---
--------------------

myTabTheme = def { fontName            = "xft:Ubuntu Mono:bold:size=11"
                 , activeColor         = "#282a36"
                 , inactiveColor       = "#44475a"
                 , activeBorderColor   = "#282a36"
                 , inactiveBorderColor = "#282a36"
                 , activeTextColor     = "#50fa7b"
                 , inactiveTextColor   = "#d0d0d0"}

tileLayout
  = renamed [Replace "tiled"]
    $ smartBorders
    $ windowNavigation
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ mySpacing
    $ ResizableTall 1 (3/100) (1/2) []
fullScreenLayout
  = renamed [Replace "Full"] Full 
tabLayout
  = renamed [Replace "tabs"]
    $ tabbed shrinkText myTabTheme
myLayoutHook
  = avoidStruts $ myDefaultLayout
  where
    myDefaultLayout
      = tileLayout
      ||| Mirror tileLayout
      ||| noBorders tabLayout
      ||| noBorders fullScreenLayout

myBitmapsDir = "/home/my_user/.xmonad/dzen2"
myDzenLogHook :: Handle -> X ()
myDzenLogHook h = dynamicLogWithPP $ def
    { ppCurrent           =   dzenColor "#50fa7b" "#000000" . pad
    , ppVisible           =   dzenColor "#8be9fd" "#000000" . pad
    , ppHidden            =   dzenColor "#6272a4" "#000000" . pad
    , ppHiddenNoWindows   =   dzenColor "#44475a" "#000000" . pad
    , ppUrgent            =   dzenColor "#ff0000" "#000000" . pad
    , ppWsSep             =   " "
    , ppSep               =   " "
    -- , ppLayout            =   dzenColor "#9c7cbf" "#000000" .
    --   (\x -> case x of
    --      "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
    --      "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
    --      "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
    --      "Simple Float"              ->      "~"
    --      _                           ->      x
    --    )
    , ppExtras            =   [ (Logr.wrapL "    " "  > " (Logr.dzenColorL "#87428A" "#000000" $ Logr.date "%R"))
                              , (Logr.wrapL "" "  > " (Logr.dzenColorL "#87428A" "#000000" $ Logr.date "%d %A %m"))
                              , (Logr.wrapL "" "  > " (Logr.dzenColorL "#87428A" "#000000" $ Logr.battery))
                                -- , (Logr.wrapL "," ")" (Logr.dzenColorL "#00AAAA" "#000000" $ (Logr.logCmd "slstatus -s")))
                              , (Logr.wrapL "" "    " (Logr.dzenColorL "#87428A" "#000000" $ (Logr.logCmd "setxkbmap -query | grep '^layout:' |awk '{print $2}'")))
                              ]
    , ppTitle             =   ("" ++) . dzenColor "#9c7cbf" "#000000" . shorten 40 . dzenEscape
    , ppOrder             =   \(ws:wtall:titl:extr) -> [ws,wtall]++extr++[titl]
    , ppOutput            =   hPutStrLn h
    }


-- draculaBackground   = 0xee282a36  -- #282a36
-- draculaSelection    = 0xff44475a  -- #44475a
-- draculaForeground   = 0xfff8f8f2  -- #f8f8f2
-- draculaComment      = 0xff6272a4  -- #6272a4
-- draculaCyan         = 0xff8be9fd  -- #8be9fd
-- draculaGreen        = 0xff50fa7b  -- #50fa7b
-- draculaOrange       = 0xffffb86c  -- #ffb86c
-- draculaPink         = 0xffff79c6  -- #ff79c6
-- draculaPurple       = 0xffbd93f9  -- #bd93f9
-- draculaRed          = 0xffff5555  -- #ff5555
-- draculaYellow       = 0xfff1fa8c  -- #f1fa8c


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


myXmobarLogHook :: Handle -> X ()
myXmobarLogHook xmproc0 = dynamicLogWithPP $ xmobarPP
              -- XMOBAR SETTINGS
              { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
                -- Current workspace
              , ppCurrent = xmobarColor sDracPink "" . wrap
                            ("<box type=Bottom width=2 mb=2 color=" ++ sDracPink ++ ">") "</box>"
                -- Visible but not current workspace
              , ppVisible = xmobarColor sDracCyan "" . wrap
                            ("<box type=Bottom width=2 mb=2 color=" ++ sDracCyan ++ ">") "</box>"
                -- Hidden workspace
              , ppHidden = xmobarColor sDracComment ""  
                -- Hidden workspaces (no windows)
              , ppHiddenNoWindows = xmobarColor sDracSelection ""  
                -- Title of active window
              , ppTitle = xmobarColor sDracComment "" . shorten 60
                -- Separator character
              , ppSep =  "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
                -- Urgent workspace
              , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
                -- Adding # of windows on current workspace to the bar
              , ppExtras  = [windowCount]
                -- order of things in xmobar
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
              }



---------------------
-- Grid Navigation --
---------------------

myGridNavigation :: GS.TwoD a (Maybe a)
myGridNavigation = GS.makeXEventhandler $ GS.shadowWithKeymap navKeyMap navDefaultHandler
 where navKeyMap
         = M.fromList [ ((0,xK_Escape), GS.cancel)
                      , ((0,xK_q)     , GS.cancel)
                      , ((0,xK_Return), GS.select)
                      , ((0,xK_m)     , GS.select)
                      , ((0,xK_slash) , GS.substringSearch myGridNavigation)
                      , ((0,xK_Left)  , GS.move   (-1, 0) >> myGridNavigation)
                      , ((0,xK_h)     , GS.move   (-1, 0) >> myGridNavigation)
                      , ((0,xK_Right) , GS.move   ( 1, 0) >> myGridNavigation)
                      , ((0,xK_l)     , GS.move   ( 1, 0) >> myGridNavigation)
                      , ((0,xK_Down)  , GS.move   ( 0, 1) >> myGridNavigation)
                      , ((0,xK_j)     , GS.move   ( 0, 1) >> myGridNavigation)
                      , ((0,xK_Up)    , GS.move   ( 0,-1) >> myGridNavigation)
                      , ((0,xK_k)     , GS.move   ( 0,-1) >> myGridNavigation)
                      , ((0,xK_space) , GS.setPos ( 0, 0) >> myGridNavigation)
                      -- emacs
                      , ((0, xK_n), GS.move ( 0, 1)  >> myGridNavigation)
                      , ((0, xK_b), GS.move (-1, 0)  >> myGridNavigation)
                      , ((0, xK_f), GS.move ( 1 ,0)  >> myGridNavigation)
                      , ((0, xK_p), GS.move ( 0,-1)  >> myGridNavigation)
                      , ((controlMask, xK_n), GS.move ( 0, 1)  >> myGridNavigation)
                      , ((controlMask, xK_p), GS.move ( 0,-1)  >> myGridNavigation)
                      , ((controlMask, xK_f), GS.move ( 1, 0)  >> myGridNavigation)
                      , ((controlMask, xK_b), GS.move (-1, 0)  >> myGridNavigation)
                      , ((controlMask, xK_m), GS.select)
                      , ((controlMask, xK_g), GS.cancel)]
       -- The navigation handler ignores unknown key symbols
       navDefaultHandler = const myGridNavigation    



word8DraculaBackground   = (0x28, 0x2a, 0x36)  -- #282a36
word8DraculaSelection    = (0x44, 0x47, 0x5a)  -- #44475a
word8DraculaForeground   = (0xf8, 0xf8, 0xf2)  -- #f8f8f2
word8DraculaComment      = (0x62, 0x72, 0xa4)  -- #6272a4
word8DraculaCyan         = (0x8b, 0xe9, 0xfd)  -- #8be9fd
word8DraculaGreen        = (0x50, 0xfa, 0x7b)  -- #50fa7b
word8DraculaOrange       = (0xff, 0xb8, 0x6c)  -- #ffb86c
word8DraculaPink         = (0xff, 0x79, 0xc6)  -- #ff79c6
word8DraculaPurple       = (0xbd, 0x93, 0xf9)  -- #bd93f9
word8DraculaRed          = (0xff, 0x55, 0x55)  -- #ff5555
word8DraculaYellow       = (0xf1, 0xfa, 0x8c)  -- #f1fa8c


-- GridSelect
-- myColorizer :: Window -> Bool -> X (String, String)
-- myColorizer = GS.colorRangeFromClassName
--               (0x28,0x2c,0x34) -- lowest inactive bg
--               (0x28,0x2c,0x34) -- highest inactive bg
--               (0xc7,0x92,0xea) -- active bg
--               (0xc0,0xa7,0x9a) -- inactive fg
--               (0x28,0x2c,0x34) -- active fg

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = GS.colorRangeFromClassName
              word8DraculaBackground -- lowest inactive bg
              word8DraculaSelection  -- highest inactive bg
              word8DraculaComment    -- active bg
              word8DraculaForeground -- inactive fg
              word8DraculaForeground -- active fg              

-- GridSelect menu layout
myGridConfig :: p -> GS.GSConfig Window
myGridConfig colorizer = (GS.buildDefaultGSConfig myColorizer)
    { GS.gs_cellheight   = 40
    , GS.gs_cellwidth    = 200
    , GS.gs_cellpadding  = 6
    , GS.gs_originFractX = 0.5
    , GS.gs_originFractY = 0.5
    , GS.gs_bordercolor  = "#44475a"
    , GS.gs_navigate     = myGridNavigation
    , GS.gs_font         ="xft:Ubuntu Mono-13"
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = GS.gridselect conf lst >>= flip whenJust spawn
    where conf = def { GS.gs_cellheight   = 40
                     , GS.gs_cellwidth    = 200
                     , GS.gs_cellpadding  = 6
                     , GS.gs_originFractX = 0.5
                     , GS.gs_originFractY = 0.5
                     , GS.gs_navigate     = myGridNavigation
                     -- , GS.gs_colorizer    = myDraculaColorizer
                     , GS.gs_bordercolor  = "#f8f8f2"
                     , GS.gs_font         = "xft:Ubuntu Mono-13"
                     }

spawnSelectedFn spawnableFn lst = GS.gridselect conf lst >>= flip whenJust spawnableFn
    where conf = def { GS.gs_cellheight   = 40
                     , GS.gs_cellwidth    = 200
                     , GS.gs_cellpadding  = 6
                     , GS.gs_originFractX = 0.5
                     , GS.gs_originFractY = 0.5
                     , GS.gs_navigate     = myGridNavigation
                     , GS.gs_bordercolor  = "#f8f8f2"
                     , GS.gs_font         = "xft:Ubuntu Mono-13"
                     }

------------------------------------------
-- Application, Configuraion, Note list --
------------------------------------------

myLinks :: [(String, String)]
myLinks = [ ("Clojure Cheat", "https://jafingerhut.github.io/cheatsheet/clojuredocs/cheatsheet-tiptip-cdocs-summary.html")
          , ("Emacs Regex", "https://www.emacswiki.org/emacs/RegularExpression")
          , ("Seesaw New", "https://cljdoc.org/d/seesaw/seesaw/1.5.0")
          , ("Seesaw Old", "http://clj-commons.org/seesaw/")
          , ("Seesaw Wiki", "https://github.com/clj-commons/seesaw/wiki")
          , ("Swing DOC", "https://docs.oracle.com/javase/7/docs/api/allclasses-noframe.html")
          ]

myProgram :: [(String, String, String)]
myProgram = [ ("Emacs"          , "emacs", "Much more than a text editor")
            , ("Emacs Ranger"   , "emacsclient -c -a emacs --eval '(ranger)'", "Client instance for emacs")
              -- web
            , ("Firefox"        , "firefox", "The famous open source web browser")
            , ("Vivaldi"        , "vivaldi-stable", "Is best configurable web browser")
            , ("Nyxt"           , "nyxt", "Web browser based on Common Lisp")
            , ("Chrome"         , "google-chrome-stable", "Google chrome browser")
            , ("Brave"          , "brave", "Brave browser")
              -- music
            , ("Spotify"        , "spotify", "Spotify music client")
            , ("MOC"            , (myTermPrompt "MOC Music" "mocp -T tty"), "MOC music client")
            , ("DeedBeaf"       , "deadbeef", "DeadBeef music player")
              -- graphics
            , ("InkScape"       , "inkscape", "Vector image editor")
            , ("Gimp"           , "gimp", "Raster image editor")
              -- uitls
            , ("Nitrogen"       , "nitrogen", "Change Wallpapers")
            , ("QBittorrent"    , "qbittorrent", "Torrent client")
            , ("DBeaver"        , "dbeaver", "Database client")
              -- mesasngers
            , ("Discord"        , "discord", "Discord client")
            , ("Mattermost"     , "mattermost-desktop", "Mattermonst Freshcode client")
            , ("Telegram"       , "telegram-desktop", "Telegram client")
              -- office
            , ("PlanMaker"      , "freeoffice-planmaker", "Sheet processor")
            , ("TextMaker"      , "freeoffice-textmaker", "Word processor")
            , ("Presenstation"  , "freeoffice-presentations", "Presentation program")
              -- work
            , ("VPN Fcode"      , (myTermPrompt "VPN FCode" ovpnFreshcode), "Freshcode VPN")
            , ("Screen Recorder", "simplescreenrecorder", "Simple Program for record screeen") 
            ]

myConfigs :: [(String, String, String)]
myConfigs = [ ("Emacs"               , myEditor ++ "~/.emacsconfig.org", "Org emacs configuration file")
            , ("XMonad"              , myEditor ++ "~/.xmonad/xmonad.hs", "Xmonad haskell file")
            , ("Dmenu Edit"          , myEditor ++ "~/.config/bin/dmenu_edit_config", "")
            , ("Dmenu Machine"       , myEditor ++ "~/.config/bin/dmenu_machine_control", "")
            , ("Dmenu Shoot"         , myEditor ++ "~/.config/bin/dmenu_shoot_app", "")
            , ("Agenda"              , myEditor ++ "~/Documents/agenda/main.org", "")
            , ("Freshcode agenda"    , myEditor ++ "~/Documents/agenda/freshcode.org", "")
            , ("English words"       , myEditor ++ "~/Documents/words.org", "English dictionary")
            , ("Atmoterm agenda"     , myEditor ++ "~/Documents/agenda/atmoterm.org", "")
            , ("Jarman agenda"       , myEditor ++ "~/programs/jarman/agenda.org", "")
            , ("Tickets backlog"     , myEditor ++ "~/tickets/thread.org", "")
            , ("Picom"               , myEditor ++ "~/.config/xdg/picom.conf", "")
            ]

myNoteDir :: [(String, String, String)]
myNoteDir = [ ("Home Ranger"   , "emacsclient -c -a emacs --eval '(ranger)'", "Client instance for emacs")
            , ("Home", "emacsclient -c -a '' ~/", "Node directory")
            , ("Documents", "emacsclient -c -a '' ~/Documents/", "Node directory")
            , ("Riser Opuses", "emacsclient -c -a '' /windows-data/opus/", "history directory")
            , ("Windows Dir", "emacsclient -c -a '' /windows-data/", "Windows directory")
            , ("Dir .bin", "emacsclient -c -a '' /home/serhii/.config/bin/", "Binary Conf")
            ]


-- for grid usage --
myProgramGrid :: [(String, String)]
myProgramGrid = [(appName, appInvoker) | (appName, appInvoker, _) <- myProgram]
myLinkGrid :: [(String, String)]
myLinkGrid = myLinks
myNoteDirGrid :: [(String, String)]
myNoteDirGrid = [(appName, appInvoker) | (appName, appInvoker, _) <- myNoteDir]
myConfigsGrid :: [(String, String)]
myConfigsGrid = [(appName, appInvoker) | (appName, appInvoker, _) <- myConfigs]

-----------------
-- Tree Select --
-----------------

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "Applications" "a list of programs I use often" (return ())) [ Node (TS.TSNode name desc (spawn app)) [] | (name, app, desc) <- myProgram]
   , Node (TS.TSNode "Configurations" "config files that edit often" (return ())) [ Node (TS.TSNode name desc (spawn app)) [] | (name, app, desc) <- myConfigs]
   , Node (TS.TSNode "Note Directories" "notes directories list"     (return ())) [ Node (TS.TSNode name desc (spawn app)) [] | (name, app, desc) <- myNoteDir]
   ]

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = draculaBackground
                              , TS.ts_font         = "xft:Ubuntu Mono-13"
                              , TS.ts_node         = (draculaForeground, draculaSelection)
                              , TS.ts_nodealt      = (draculaForeground, draculaSelection)
                              , TS.ts_highlight    = (draculaForeground, draculaComment)
                              , TS.ts_extra        = draculaComment
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 30
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation}

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),             TS.cancel)
    , ((0, xK_q),                  TS.cancel)
    , ((controlMask, xK_g),        TS.cancel)
    , ((0, xK_m),                  TS.select)
    , ((0, xK_Return),             TS.select)
    , ((0, xK_space),              TS.select)
    , ((0, xK_Up),                 TS.movePrev)
    , ((0, xK_Down),               TS.moveNext)
    , ((0, xK_Left),               TS.moveParent)
    , ((0, xK_Right),              TS.moveChild)
    -- vim 
    , ((0, xK_k),                  TS.movePrev)
    , ((0, xK_j),                  TS.moveNext)
    , ((0, xK_h),                  TS.moveParent)
    , ((0, xK_l),                  TS.moveChild)
    -- emacs
    , ((0, xK_n),                  TS.moveNext)
    , ((0, xK_b),                  TS.moveParent)
    , ((0, xK_f),                  TS.moveChild)
    , ((0, xK_p),                  TS.movePrev)
    , ((controlMask, xK_n),        TS.moveNext)
    , ((controlMask, xK_p),        TS.movePrev)
    , ((controlMask, xK_f),        TS.moveChild)
    , ((controlMask, xK_b),        TS.moveParent)
    , ((controlMask, xK_m),        TS.select)
    ]


----------------
-- keybinding --
----------------

-- myKeys =
--     -- Main Keyset 
--   [ ("M-<Space>"   , sendMessage NextLayout)
--   , ("M-<Tab>"     , sendMessage $ JumpToLayout "Full")
--   , ("M-C-<Space>" , spawn "/home/serhii/.config/bin/layout_keyboard_switcher.sh")
--   , ("M-<Escape>"  , spawn "/home/serhii/.config/bin/layout_keyboard_switcher.sh")
--   , ("M-a"   , spawnSelected' myProgramGrid)
--   , ("M-c"   , spawnSelected' myConfigsGrid)
--   , ("M-o"   , spawnSelected' myNoteDirGrid)
--   , ("M-f"   , GS.goToSelected  $ myGridConfig myColorizer)
--   , ("M-S-f" , GS.bringSelected $ myGridConfig myColorizer)
--   , ("M-S-n" , spawn "nautilus -w")
--   , ("M-S-m" , spawnAndDo (doCenterFloat) "urxvt")
--   , ("M-S-s" , spawn "gnome-screenshot -i")
--     --, ("M-S-p" , treeselectAction tsDefaultConfig)  
--     --, ("M-S d", spawn "emacsclient -c -a '' --eval '(dired nil)'")
--     -- Dmenu
--   , ("M-p"   , spawn dmenu)
--   , ("M-C-f" , spawn dmenufm)
--   , ("M-C-k" , spawn dmenuKillProcess)
--   , ("M-C-e" , spawn dmenuEditConfig)
--   , ("M-C-p" , spawn dmenuShootUp)
--   , ("M-C-m" , spawn dmenuMachineControl)
--   , ("M-C-s" , spawn dmenuSoundControl)
--   , ("M-C-n" , spawn dmenuNetworkManager)
--   , ("M-C-r" , spawn "setxkbmap ru")
--     -- , ("M-b"   , sendMessage ToggleStruts)
--   ]
--   ++ -- System controls
--   [ ("<XF86AudioRaiseVolume>" , spawn "amixer -q sset Master 5%+")
--   , ("<XF86AudioLowerVolume>" , spawn "amixer -q sset Master 5%-")
--   , ("<XF86AudioMute>"        , spawn "amixer set Master toggle")
--   , ("<XF86MonBrightnessUp>"  , Bright.increase)
--   , ("<XF86MonBrightnessDown>", Bright.decrease)
--   ]
--   ++ -- Music control
--   [ ("<XF86AudioNext>", spawn "/home/serhii/.xmonad/spotify_control next")
--   , ("<XF86AudioPrev>", spawn "/home/serhii/.xmonad/spotify_control previous")
--   , ("<XF86AudioStop>", spawn "/home/serhii/.xmonad/spotify_control playpause")
--   , ("<XF86AudioPlay>", spawn "/home/serhii/.xmonad/spotify_control playpause")
--   , ("M-<XF86AudioNext>", spawn "mocp -f")
--   , ("M-<XF86AudioPrev>", spawn "mocp -r")
--   , ("M-<XF86AudioStop>", spawn "mocp -G")
--   , ("M-<XF86AudioPlay>", spawn "mocp -G")
--   ]


-- myTraditional =
--   [
--     ((myModMask, xK_Escape)            , spawn "/home/serhii/.config/bin/layout_keyboard_switcher.sh"),
--     ((myModMask, xK_p)                 , spawn dmenu),
--     ((myModMask .|. controlMask, xK_f) , spawn dmenufm),
--     ((myModMask .|. controlMask, xK_k) , spawn dmenuKillProcess),
--     ((myModMask .|. controlMask, xK_e) , spawn dmenuEditConfig),
--     ((myModMask .|. controlMask, xK_p) , spawn dmenuShootUp),
--     ((myModMask .|. controlMask, xK_m) , spawn dmenuMachineControl),
--     ((myModMask .|. controlMask, xK_s) , spawn dmenuSoundControl),
--     ((myModMask .|. controlMask, xK_n) , spawn dmenuNetworkManager)
--   ]
--   ++ -- System controls
--   [
--     ((myModMask .|. shiftMask, xK_z)     , spawn "xscreensaver-command -lock"),
--     ((0, 0x1008FF11)                     , spawn "amixer -q sset Master 5%-"),
--     ((0, 0x1008FF13)                     , spawn "amixer -q sset Master 5%+"),
--     ((0, 0x1008FF12)                     , spawn "amixer set Master toggle")
--     ((0, XF86.xF86XK_MonBrightnessUp)    , Bright.increase)
--     ((0, XF86.xF86XK_MonBrightnessDown)  , Bright.decrease)
--   ]

myKeys =
    -- Main Keyset 
  [ ("M-<Space>"   , sendMessage NextLayout)
  , ("M-<Tab>"     , sendMessage $ JumpToLayout "Full")
  , ("M-C-<Space>" , spawn "/home/serhii/.config/bin/layout_keyboard_switcher.sh")
  , ("M-<Escape>"  , spawn "/home/serhii/.config/bin/layout_keyboard_switcher.sh")
  , ("M-a"   , spawnSelected' myProgramGrid)
  , ("M-c"   , spawnSelected' myConfigsGrid)
  , ("M-o"   , spawnSelected' myNoteDirGrid)
  , ("M-s l" , spawnSelectedFn (\url -> spawn ("xdg-open \"" ++ url ++ "\"")) myLinkGrid)
  , ("M-f"   , GS.goToSelected  $ myGridConfig myColorizer)
  , ("M-S-f" , GS.bringSelected $ myGridConfig myColorizer)
  , ("M-S-n" , spawn "nautilus -w")
  , ("M-S-m" , spawnAndDo (doCenterFloat) "urxvt")
  -- , ("M-S-s" , spawn "gnome-screenshot -i")
  , ("M-S-s", spawn "flameshot gui")
    --, ("M-S-p" , treeselectAction tsDefaultConfig)  
    --, ("M-S d", spawn "emacsclient -c -a '' --eval '(dired nil)'")
    -- Dmenu
  , ("M-p"   , spawn dmenu)
  , ("M-C-f" , spawn dmenufm)
  , ("M-C-k" , spawn dmenuKillProcess)
  , ("M-C-e" , spawn dmenuEditConfig)
  , ("M-C-p" , spawn dmenuShootUp)
  , ("M-C-m" , spawn dmenuMachineControl)
  , ("M-C-s" , spawn dmenuSoundControl)
  , ("M-C-n" , spawn dmenuNetworkManager)
  , ("M-C-r" , spawn "setxkbmap ru")
    -- , ("M-b"   , sendMessage ToggleStruts)
  ]
  ++ -- System controls
  [ ("<XF86AudioRaiseVolume>" , spawn "amixer -q sset Master 5%+")
  , ("<XF86AudioLowerVolume>" , spawn "amixer -q sset Master 5%-")
  , ("<XF86AudioMute>"        , spawn "amixer set Master toggle")
  , ("<XF86MonBrightnessUp>"  , Bright.increase)
  , ("<XF86MonBrightnessDown>", Bright.decrease)
  ]
  ++ -- Music control
  [ ("<XF86AudioNext>", spawn "/home/serhii/.xmonad/spotify_control next")
  , ("<XF86AudioPrev>", spawn "/home/serhii/.xmonad/spotify_control previous")
  , ("<XF86AudioStop>", spawn "/home/serhii/.xmonad/spotify_control playpause")
  , ("<XF86AudioPlay>", spawn "/home/serhii/.xmonad/spotify_control playpause")
  , ("M-<XF86AudioNext>", spawn "deadbeef --next")
  , ("M-<XF86AudioPrev>", spawn "deadbeef --prev")
  , ("M-<XF86AudioStop>", spawn "deadbeef --toggle-pause")
  , ("M-<XF86AudioPlay>", spawn "deadbeef --toggle-pause")
  -- , ("M-<XF86AudioNext>", spawn "mocp -f")
  -- , ("M-<XF86AudioPrev>", spawn "mocp -r")
  -- , ("M-<XF86AudioStop>", spawn "mocp -G")
  -- , ("M-<XF86AudioPlay>", spawn "mocp -G")
  ]
