-- -*- mode: haskell; mode: rainbow; -*-
-- __  ____  __  ___  _   _    _    ____
-- \ \/ /  \/  |/ _ \| \ | |  / \  |  _ \
--  \  /| |\/| | | | |  \| | / _ \ | | | |
--  /  \| |  | | |_| | |\  |/ ___ \| |_| |
-- /_/\_\_|  |_|\___/|_| \_/_/   \_\____/
--
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

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell (shellPrompt, safePrompt, prompt)

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
import XMonad.Hooks.ManageDocks (docks, avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat, isDialog)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.LayoutCombinators ((|||), JumpToLayout(JumpToLayout))
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

-------------------
-- Color Schemas --
-------------------
--
-- For gridselection you must use colors in (Word8, Word8, Word8), rgb tuple.
-- For easy generation tuple list you can use `replace-regexp`. Just keep all
-- colors in unificated notation, example:
--   colorDarkBlue  = "#282a36"
--
-- RE-PATTERN:  color\([[:alpha:]]+\)\([[:blank:]]+=[[:blank:]]+\)"#\(..\)\(..\)\(..\)"
-- RE-REPLACE:  colorWord8\1\2(0x\3, 0x\4, 0x\5) -- #\3\4\5


-- * Dracula * --

colorBackground   = "#282a36"
colorSelection    = "#44475a"
colorForeground   = "#f8f8f2"
colorComment      = "#6272a4"
colorCyan         = "#8be9fd"
colorGreen        = "#50fa7b"
colorOrange       = "#ffb86c"
colorPink         = "#ff79c6"
colorLightPink    = "#ff3e96"
colorPurple       = "#bd93f9"
colorViolet       = "#87428a"
colorRed          = "#ff5555"
colorYellow       = "#f1fa8c"
colorWhite        = "#ffffff"
colorBlack        = "#000000"
color'bordr'norm  = colorBackground
color'bordr'focs  = "#ca0066"
color'dmenu'nb    = colorBlack
color'dmenu'nf    = "#f9f9f9"
color'dmenu'sb    = colorViolet
color'dmenu'sf    = colorWhite
color'dmenu'mf    = colorWhite
color'dmenu'uf    = colorLightPink

colorWord8Background   = (0x28, 0x2a, 0x36) -- #282a36
colorWord8Selection    = (0x44, 0x47, 0x5a) -- #44475a
colorWord8Foreground   = (0xf8, 0xf8, 0xf2) -- #f8f8f2
colorWord8Comment      = (0x62, 0x72, 0xa4) -- #6272a4

----------------
-- Properties --
----------------

  -- Terminal configurations
terminal'default = "/home/serhii/.xmonad/run_custom_terminal.sh"
terminal'urxvt title command = "urxvt -title \""++ title ++"\" -e sh -c "++ command
terminal'gnome'output title command = "gnome-terminal --profile='Red Alert' --title=\""++ title ++"\" --command "++ command
terminal'gnome'progrm title command = "gnome-terminal --profile='dracula' --title=\""++ title ++"\" --command "++ command
terminal'gnome title command = "$HOME/.xmonad/run_custom_terminal_command.sh '" ++ title ++ "' '"++ command ++ "'"
  -- Scripts
script'RunVPN            = "$HOME/fcode/docs/run_vpn.sh"
script'SwitchLang        = "/home/serhii/.config/bin/layout_keyboard_switcher.sh"
  -- emacs
emacs'editor             = "emacsclient -c -a emacs "
emacs'dired              = "emacsclient -c -a '' --eval '(dired nil)'"
emacs'ranger             = "emacsclient -c -a emacs --eval '(ranger)'"
emacs'eshell             = "emacsclient -c -a '' --eval \"(progn (eshell) (set-window-margins (selected-window) 3 3) (set-window-fringes (selected-window) 0 0 nil))\""
  -- Dmenu
dmenu'fm                 = "dmenufm"
dmenu'KillProcess        = "dmenu_kill_process"
dmenu'ShootUp            = "dmenu_shoot_app"
dmenu'EditConfig         = "dmenu_edit_config"
dmenu'MachineControl     = "dmenu_machine_control"
dmenu'SoundControl       = "dmenu_sound_control"
dmenu'NetworkManager     = "networkmanager_dmenu"
dmenu                    = "dmenu_run -fn 'UbuntuMono:style=bold:size=12' -h 30 "
                           ++ "-nb '" ++ color'dmenu'nb ++ "' " -- normal background
                           ++ "-nf '" ++ color'dmenu'nf ++ "' " -- normal foreground
                           ++ "-sb '" ++ color'dmenu'sb ++ "' " -- selected background
                           ++ "-sf '" ++ color'dmenu'sf ++ "' " -- selected font 
                           ++ "-mf '" ++ color'dmenu'mf ++ "' " -- fuzzy matched font
                           ++ "-uf '" ++ color'dmenu'uf ++ "'"  -- fuzzy unmatched font

----------
-- Main --
----------

main = do
  xmproc0 <- spawnPipe ("xmobar -x 0 $HOME/.xmonad/xmobarrc_dracula")
  xmonad
    $ docks
    $ ewmhFullscreen
    $ ewmh
    $ def
    { terminal            = terminal'default
    , modMask             = mod4Mask
    , workspaces          = ["1", "2", "web", "other", "media", "messages"] ++ (map show [7..8]) ++ ["output"]
    , layoutHook          = showWName' layout'currentWorkSpaceDialogTheme $ layout'layoutHook
    , manageHook          = hook'WindowManage
    , startupHook         = hook'StartUp
    , borderWidth         = 0
    , normalBorderColor   = color'bordr'norm
    , focusedBorderColor  = color'bordr'focs
    , logHook             = xmobar'LogHook xmproc0 >> fadeWindowsLogHook hook'WindowFade
    } `additionalKeysP` keybindingSettings

--------------------
-- Behavior hooks --
--------------------

hook'StartUp :: X ()
hook'StartUp = do
  spawnOnce "/usr/bin/emacs --daemon &"
  spawnOnce "picom --config ~/.config/xdg/picom.conf &"
  setWMName "LG3D"

hook'WindowFade = composeAll
    [ opaque
    , isUnfocused --> opacity 0.95
    , isDialog --> opaque
    , (className =? "Vivaldi-stable") <&&> (isUnfocused) --> opacity 1.0
    , (className =? "firefox") <&&> (isUnfocused) --> opacity 0.97
    , (className =? "Totem") <&&> (isUnfocused) --> opacity 1.0
    , (className =? "Eog") <&&> (isUnfocused) --> opacity 1.0
    , (title =? "Picture in picture") <&&> (isUnfocused) --> opacity 1.0
    , fmap ("Google" `S.isPrefixOf`) className --> opaque
    --, isFloating  --> opacity 0.75
    ]

hook'WindowManage :: ManageHook
hook'WindowManage = composeAll
  [ className =? "confirm"              --> doFloat
  , className =? "file_progress"        --> doFloat
  , className =? "dialog"               --> doFloat
  , className =? "download"             --> doFloat
  , className =? "error"                --> doFloat
  , className =? "notification"         --> doFloat
  , className =? "splash"               --> doFloat
  , className =? "toolbar"              --> doFloat
  , className =? "Eog"                  --> doFloat
  , className =? "Gthumb"               --> doFloat
  , className =? "gnome-calculator"     --> doFloat
  , className =? "Nm-connection-editor" --> doFloat
  , title     =? "Jarman"               --> doFloat
  , title     =? "Picture in picture"   --> doFloat
  , title     =? "Incanter Plot"        --> doFloat
  , title     =? "Search"               --> doCenterFloat
  , className =? "Gnome-screenshot"     --> doCenterFloat
  , className =? "Org.gnome.Nautilus"   --> doCenterFloat
  , className =? "SimpleScreenRecorder" --> doCenterFloat
  ] <+> manageDocks

--------------------
--- Layout Hooks ---
--------------------

-- small frame which idicate currently
-- switched workspace on screen
layout'currentWorkSpaceDialogTheme :: SWNConfig
layout'currentWorkSpaceDialogTheme
  = def { swn_font      = "xft:Ubuntu Mono:bold:size=60"
        , swn_fade      = 0.3
        , swn_bgcolor   = colorBackground
        , swn_color     = colorForeground
        }

  -- Configuration of window spacing'
  -- border screen size for all monitor;
layout'spacing'BorderScreenSize :: Integer
layout'spacing'BorderScreenSize = 0
  -- border for each window in screen;
layout'spacing'BorderWindowSize :: Integer
layout'spacing'BorderWindowSize = 13
  -- defing self border set;
layout'spacing'ScreenBorder = Border layout'spacing'BorderScreenSize layout'spacing'BorderScreenSize layout'spacing'BorderScreenSize layout'spacing'BorderScreenSize
layout'spacing'WindowBorder = Border layout'spacing'BorderWindowSize layout'spacing'BorderWindowSize layout'spacing'BorderWindowSize layout'spacing'BorderWindowSize
  -- creating function for layout modifier
layout'spacing'config :: (l a -> ModifiedLayout Spacing l a)
layout'spacing'config = spacingRaw True layout'spacing'ScreenBorder True layout'spacing'WindowBorder True

layout'mode'tile
  = renamed [Replace "tiled"]
    $ smartBorders
    $ windowNavigation
    $ subLayout [] (smartBorders Simplest)
    $ limitWindows 12
    $ layout'spacing'config
    $ ResizableTall 1 (3/100) (1/2) []
layout'mode'fullScreen
  = renamed [Replace "Full"] Full
layout'mode'tabs
  = renamed [Replace "tabs"]
    $ tabbed shrinkText
    (def { fontName            = "xft:Ubuntu Mono:bold:size=11"
         , activeColor         = colorBackground
         , inactiveColor       = colorSelection
         , activeBorderColor   = colorBackground
         , inactiveBorderColor = colorBackground
         , activeTextColor     = colorLightPink
         , inactiveTextColor   = colorForeground})
layout'layoutHook
  = avoidStruts $ myDefaultLayout
  where
    myDefaultLayout
      = layout'mode'tile
      ||| Mirror layout'mode'tile
      ||| noBorders layout'mode'tabs
      ||| noBorders layout'mode'fullScreen

xmobar'LogHook :: Handle -> X ()
xmobar'LogHook xmproc0 = dynamicLogWithPP $ xmobarPP
  { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
  -- Current workspace
  , ppCurrent = xmobarColor colorPink "" . wrap
                ("<box type=Bottom width=2 mb=2 color=" ++ colorPink ++ ">") "</box>"
                -- Visible but not current workspace
  , ppVisible = xmobarColor colorCyan "" . wrap
                ("<box type=Bottom width=2 mb=2 color=" ++ colorCyan ++ ">") "</box>"
  -- Hidden workspace
  , ppHidden = xmobarColor colorComment ""
  -- Hidden workspaces (no windows)
  , ppHiddenNoWindows = xmobarColor colorSelection ""
  -- Title of active window
  , ppTitle = xmobarColor colorComment "" . shorten 60
  -- Separator character
  , ppSep =  "<fc=" ++ colorSelection ++ "> <fn=1>:</fn> </fc>"
  -- Urgent workspace
  , ppUrgent = xmobarColor colorRed "" . wrap "!" "!"
  -- Adding # of windows on current workspace to the bar
  -- order of things in xmobar
  , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
  -- aliasing for layout names
  , ppLayout =
      (\x -> case x of
          "tiled"          -> "c"
          "Mirror tiled"   -> "m"
          "tabs"           -> "t"
          "Full"           -> "f"
          _                -> x
      )
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


gridWindowColorizer :: Window -> Bool -> X (String, String)
gridWindowColorizer = GS.colorRangeFromClassName
  colorWord8Background -- lowest inactive bg
  colorWord8Selection  -- highest inactive bg
  colorWord8Comment    -- active bg
  colorWord8Foreground -- inactive fg
  colorWord8Foreground -- active fg

-- GridSelect menu layout
myGridConfig :: GS.GSConfig Window
myGridConfig = (GS.buildDefaultGSConfig gridWindowColorizer)
    { GS.gs_cellheight   = 40
    , GS.gs_cellwidth    = 200
    , GS.gs_cellpadding  = 6
    , GS.gs_originFractX = 0.5
    , GS.gs_originFractY = 0.5
    , GS.gs_bordercolor  = colorBackground
    , GS.gs_navigate     = myGridNavigation
    , GS.gs_font         ="xft:Ubuntu Mono-13"
    }

spawnGridSelected :: [(String, String)] -> X ()
spawnGridSelected lst = GS.gridselect conf lst >>= flip whenJust spawn
    where conf = def { GS.gs_cellheight   = 40
                     , GS.gs_cellwidth    = 180
                     , GS.gs_cellpadding  = 6
                     , GS.gs_originFractX = 0.5
                     , GS.gs_originFractY = 0.5
                     , GS.gs_navigate     = myGridNavigation
                     , GS.gs_bordercolor  = colorSelection -- "#333344"
                     , GS.gs_font         = "xft:Ubuntu Mono-13"
                     }


spawnGridSelectedFn lst spawnableFn = GS.gridselect conf lst >>= flip whenJust spawnableFn
    where conf = def { GS.gs_cellheight   = 40
                     , GS.gs_cellwidth    = 200
                     , GS.gs_cellpadding  = 6
                     , GS.gs_originFractX = 0.5
                     , GS.gs_originFractY = 0.5
                     , GS.gs_navigate     = myGridNavigation
                     , GS.gs_bordercolor  = colorSelection -- "#f8f8f2"
                     , GS.gs_font         = "xft:Ubuntu Mono-13"
                     }

------------------------------------------
-- Application, Configuraion, Note list --
------------------------------------------

listWebLinks :: [(String, String)]
listWebLinks =
  [ ("Clojure Cheat", "https://jafingerhut.github.io/cheatsheet/clojuredocs/cheatsheet-tiptip-cdocs-summary.html")
  , ("Emacs Regex", "https://www.emacswiki.org/emacs/RegularExpression")
  , ("Seesaw New", "https://cljdoc.org/d/seesaw/seesaw/1.5.0")
  , ("Seesaw Old", "http://clj-commons.org/seesaw/")
  , ("Seesaw Wiki", "https://github.com/clj-commons/seesaw/wiki")
  , ("Swing DOC", "https://docs.oracle.com/javase/7/docs/api/allclasses-noframe.html")
  ]

listPrograms :: [(String, String, String)]
listPrograms =
  [ ("Emacs"          , "emacs", "Much more than a text editor")
  , ("Emacs Ranger"   , "emacsclient -c -a emacs --eval '(ranger)'", "Client instance for emacs")
    -- web
  , ("Firefox"        , "firefox", "The famous open source web browser")
  , ("Vivaldi"        , "vivaldi-stable", "Is best configurable web browser")
  , ("Nyxt"           , "nyxt", "Web browser based on Common Lisp")
  , ("Chrome"         , "google-chrome-stable", "Google chrome browser")
    -- music
  , ("Spotify"        , "spotify", "Spotify music client")
  , ("DeedBeaf"       , "deadbeef", "DeadBeef music player")
    -- graphics
  , ("InkScape"       , "inkscape", "Vector image editor")
  , ("Gimp"           , "gimp", "Raster image editor")
    -- uitls
  , ("Nitrogen"       , "nitrogen", "Change Wallpapers")
  , ("QBittorrent"    , "qbittorrent", "Torrent client")
  , ("DataGrip"       , "/home/serhii/.prog-install/DataGrip/bin/datagrip.sh", "DataGrip")
  , ("DBeaver"        , "dbeaver", "Database client")
    -- messangers
  , ("Discord"        , "discord", "Discord client")
  , ("Mattermost"     , "mattermost-desktop", "Mattermonst Freshcode client")
  , ("Telegram"       , "telegram-desktop", "Telegram client")
    -- office
  , ("PlanMaker"      , "freeoffice-planmaker", "Sheet processor")
  , ("TextMaker"      , "freeoffice-textmaker", "Word processor")
  , ("Presenstation"  , "freeoffice-presentations", "Presentation program")
    -- Another
  , ("VPN Fcode"      , (terminal'gnome'output "VPN FreshCode" script'RunVPN), "Freshcode VPN")
  , ("HTop"           , (terminal'gnome "Resource Monitor" "htop"), "System resource monitoring")
  , ("Screen Recorder", "simplescreenrecorder", "Simple Program for record screeen")
  , ("Flameshot"      , "flameshot gui", "Screenshot program")
  , ("Gromit"         , "gromit-mpx", "Screendrawing tool F9 to activate")
  , ("ARandR"         , "arandr", "quick screen manager")
  , ("NetManager"     , "nm-connection-editor", "Network Manager GUI")
  , ("GDisks"         , "gnome-disks", "Disk and Partition manager")
  , ("Calculator"     , "gnome-calculator", "Disk and Partition manager")
  -- , ("Sys Monitor"    , "gnome-system-monitor", "System monitor")
  -- , ("Sys Logs"       , "gnome-system-log", "System Logs")
  -- , ("Font Manager"   , "font-manager", "Font Manager")
  -- , ("Kleopatra"      , "kleopatra", "Key manager")
  -- , ("Evince"         , "evince", "PDF viewer")
  ]

-- TODO:
-- [ ] add list from the direcotries
listConfigs :: [(String, String, String)]
listConfigs =
  [ ("Emacs"               , emacs'editor ++ "~/.emacsconfig.org", "Org emacs configuration file")
  , ("XMonad"              , emacs'editor ++ "~/.xmonad/xmonad.hs", "Xmonad haskell file")
  , ("Dmenu Edit"          , emacs'editor ++ "~/.config/bin/dmenu_edit_config", "")
  , ("Dmenu Machine"       , emacs'editor ++ "~/.config/bin/dmenu_machine_control", "")
  , ("Dmenu Shoot"         , emacs'editor ++ "~/.config/bin/dmenu_shoot_app", "")
  , ("Agenda"              , emacs'editor ++ "~/Documents/agenda/main.org", "")
  , ("Freshcode agenda"    , emacs'editor ++ "~/Documents/agenda/freshcode.org", "")
  , ("English words"       , emacs'editor ++ "~/Documents/words.org", "English dictionary")
  , ("Atmoterm agenda"     , emacs'editor ++ "~/Documents/agenda/atmoterm.org", "")
  , ("Jarman agenda"       , emacs'editor ++ "~/programs/jarman/agenda.org", "")
  , ("Tickets backlog"     , emacs'editor ++ "~/tickets/thread.org", "")
  , ("Picom"               , emacs'editor ++ "~/.config/xdg/picom.conf", "")
  ]

-- TODO:
-- [ ] rework to two level notes gridmap
listNoteDir :: [(String, String, String)]
listNoteDir =
  [ ("Home Ranger"   , "emacsclient -c -a emacs --eval '(ranger)'", "Client instance for emacs")
  , ("Home", "emacsclient -c -a '' ~/", "Node directory")
  , ("Documents", "emacsclient -c -a '' ~/Documents/", "Node directory")
  , ("Riser Opuses", "emacsclient -c -a '' /windows-data/opus/", "history directory")
  , ("Windows Dir", "emacsclient -c -a '' /windows-data/", "Windows directory")
  , ("Dir .bin", "emacsclient -c -a '' /home/serhii/.config/bin/", "Binary Conf")
  ]

myProgramGrid :: [(String, String)]
myProgramGrid  = [(appName, appInvoker) | (appName, appInvoker, _) <- listPrograms]
myLinkGrid    :: [(String, String)]
myLinkGrid     = listWebLinks
myNoteDirGrid :: [(String, String)]
myNoteDirGrid  = [(appName, appInvoker) | (appName, appInvoker, _) <- listNoteDir]

------------
-- Propmt --
------------

myPromptConfig :: XPConfig
myPromptConfig = def
      { font                  = "xft:Ubuntu Mono-13"
      , bgColor               = colorBackground
      , fgColor               = colorForeground
      , bgHLight              = colorComment
      , fgHLight              = colorForeground
      , borderColor           = colorBackground
      , promptBorderWidth     = 0
      , promptKeymap          = defaultXPKeymap
      , completionKey         = (0,xK_Tab)
      , changeModeKey         = xK_grave
      , position              = Top
      , height                = 20
      , maxComplRows          = Just 10
      , maxComplColumns       = Nothing
      , historySize           = 256
      , historyFilter         = id
      , defaultText           = []
      , autoComplete          = Nothing
      , showCompletionOnTab   = False
      , complCaseSensitivity  = CaseSensitive
      -- , searchPredicate       = isPrefixOf
      , alwaysHighlight       = True
      , defaultPrompter       = id
      , sorter                = const id
      }

calcPrompt :: XPConfig -> String -> X ()
calcPrompt xpcfg ans =
  inputPrompt xpcfg (trim ans) ?+ \input ->
      liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt xpcfg
  where
    trim = f . f
      where f = reverse . dropWhile isSpace

babashkaPrompt :: XPConfig -> String -> X ()
babashkaPrompt xpcfg ans =
  inputPrompt xpcfg (trim ans) ?+ \input ->
      liftIO(runProcessWithInput "BB" [input] "") >>= calcPrompt xpcfg
  where
    trim = f . f
      where f = reverse . dropWhile isSpace

----------------
-- keybinding --
----------------

keybindingSettings =
  -- Main Keyset
  [ ("M-<Space>"   , sendMessage NextLayout)
  , ("M-<Tab>"     , sendMessage $ JumpToLayout "Full")
  , ("M-C-<Space>" , spawn script'SwitchLang)
  , ("M-<Escape>"  , spawn script'SwitchLang)
  , ("M-i"         , calcPrompt myPromptConfig "Math") -- ("M-i"   , shellPrompt myPromptConfig)
  , ("M-C-t"       , spawn emacs'eshell)
  ]
  ++ -- Grid Select
  [ ("M-a"   , spawnGridSelected myProgramGrid)
  , ("M-s l" , spawnGridSelectedFn myLinkGrid (\url -> spawn ("xdg-open \"" ++ url ++ "\"")))
  , ("M-f"   , GS.goToSelected  $ myGridConfig)
  , ("M-S-f" , GS.bringSelected $ myGridConfig)
  ]
  ++ -- Programs
  [ ("M-S-n" , spawn "nautilus -w")
  , ("M-S-m" , spawnAndDo (doCenterFloat) "urxvt")
  , ("M-S-f" , spawn emacs'ranger)
  , ("M-S-s" , spawn "flameshot gui")
  ]
  ++ -- Dmenu
  [ ("M-p"   , spawn dmenu)
  , ("M-C-f" , spawn dmenu'fm)
  , ("M-C-k" , spawn dmenu'KillProcess)
  , ("M-C-e" , spawn dmenu'EditConfig)
  , ("M-C-p" , spawn dmenu'ShootUp)
  , ("M-C-m" , spawn dmenu'MachineControl)
  , ("M-C-s" , spawn dmenu'SoundControl)
  , ("M-C-n" , spawn dmenu'NetworkManager)
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
