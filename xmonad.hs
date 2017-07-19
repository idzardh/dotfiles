import XMonad                        hiding ( (|||) )
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.NoBorders              ( smartBorders )
import XMonad.Util.EZConfig                 ( additionalKeysP )
import XMonad.Util.Run                      ( runInTerm, spawnPipe, hPutStrLn )
import XMonad.Util.Themes
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog

import qualified XMonad.StackSet as W

import qualified Data.Map as M

-----------------------------------------------------------------------------}}}
-- Main                                                                      {{{
--------------------------------------------------------------------------------

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad
    -- $ dynamicProjects projects
    $ def
      { terminal           = myTerminal
      , modMask            = myModMask
      , borderWidth        = myBorderWidth
      , normalBorderColor  = "#111111"
      , focusedBorderColor = "#2c8fa0"
      , layoutHook         = avoidStruts $ smartBorders $ myLayouts
      , handleEventHook    = handleEventHook def <+> docksEventHook
      , startupHook        = myStartupHook
      , logHook            = dynamicLogWithPP $ xmobarPP
        { ppOutput    = hPutStrLn xmproc
        , ppCurrent   = xmobarColor "#2c8fa0" "" . wrap "[" "]"
        , ppSep       = " | "
        , ppLayout    = (\x -> case (last . words) x of
                            "Tall"       -> " <fn=1>\xf04c</fn> "
                            "ThreeCol"   -> " <fn=1>\xe0cf</fn> "
                            "Full"       -> " <fn=1>\xf0c8</fn> "
                            _            -> x
                        )
        , ppTitle     = xmobarColor "#2c8fa0" "" . shorten 50
        }
      , workspaces         = myWorkspaces
      , manageHook         = myManageHook <+> manageHook def
      }
      `additionalKeysP` myAdditionalKeys

-----------------------------------------------------------------------------}}}
-- Simple configuration variable                                             {{{
--------------------------------------------------------------------------------

myTerminal    = "tilix"
myModMask     = mod4Mask
myBorderWidth = 2
myLayout      = Tall 1 (3/100) (1/2)
myBrowser     = "firefox"

mySpacing :: Int
mySpacing     = 5
noSpacing :: Int
noSpacing     = 0

-----------------------------------------------------------------------------}}}
-- Layouts                                                                   {{{
--------------------------------------------------------------------------------

-- better specify layouts
myLayouts = spacing mySpacing $ myLayout ||| ThreeColMid 1 (3/100) (1/2) ||| ThreeCol 1 (3/100) (1/3) ||| Full

-- addTopBar = noFrillsDeco shrinkText topBarTheme

-----------------------------------------------------------------------------}}}
-- Themes & Colours                                                          {{{
--------------------------------------------------------------------------------

topBarTheme = def
  { fontName              = myFont
  , inactiveBorderColor   = base03
  , inactiveColor         = base03
  , inactiveTextColor     = base03
  , activeBorderColor     = active
  , activeColor           = active
  , activeTextColor       = active
  , urgentBorderColor     = red
  , urgentTextColor       = yellow
  , decoHeight            = topbar
 }

-- fonts
myFont = "xft:UbuntuMonoDerivativePowerline Nerd Font Regular:size=9"

-- sizes
topbar    = 10

-- colours for themes
active       = blue
activeWarn   = red
inactive     = base02
focusColor   = blue
unfocusColor = base02

-- colours (Thanks to Altercation!)
base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

-----------------------------------------------------------------------------}}}
-- Workspaces                                                                {{{
--------------------------------------------------------------------------------

-- fa icons!
wsGEN = "1: <fn=1>\xf269</fn> "
wsWRK = "2: <fn=1>\xf02d</fn> "
wsSYS = "3: <fn=1>\xf300</fn> "
wsMED = "4: <fn=1>\xf001</fn> "
wsTMP = "5: <fn=1>\xf103</fn> "
wsGAM = "6: <fn=1>\xf11b</fn> "

myWorkspaces :: [String]
myWorkspaces = [wsGEN, wsWRK, wsSYS, wsMED, wsTMP, wsGAM]

-----------------------------------------------------------------------------}}}
-- Projects                                                                  {{{
--------------------------------------------------------------------------------

projects :: [Project]
projects =
  [ Project { projectName       = wsGEN
            , projectDirectory  = "~/"
            , projectStartHook  = Nothing
            }

  , Project { projectName       = wsWRK
            , projectDirectory  = "~/Documents/studie"
            , projectStartHook  = Just $ do spawnOn wsWRK myTerminal
                                            spawnOn wsWRK myBrowser
            }

  , Project { projectName       = wsSYS
            , projectDirectory  = "~/"
            , projectStartHook  = Just $ do spawnOn wsSYS myTerminal
                                            runInTerm "" "htop"
                                            spawnOn wsSYS myTerminal
            }

  , Project { projectName       = wsMED
            , projectDirectory  = "~/"
            , projectStartHook  = Just $ do spawnOn wsMED myTerminal
                                            spawnOn wsMED myTerminal
                                            spawnOn wsMED myBrowser
            }

  , Project { projectName       = wsTMP
            , projectDirectory  = "~/"
            , projectStartHook  = Just $ do return ()
            }
  ]

-----------------------------------------------------------------------------}}}
-- Keys                                                                      {{{
--------------------------------------------------------------------------------

--TODO: addName?
--TODO: audio function keys
--TODO: move some programs automatically to workspaces
--TODO: split keys in different functionality (system, media, launchers)
myAdditionalKeys =
  [ ("M-z"        , spawn "tilix -e vim -o ~/Dropbox/todo.txt/todo.txt ~/Dropbox/todo.txt/done.txt")
  , ("M-S-z"      , spawn "~/dotfiles/scripts/lockscreen.sh")
  , ("M-s"        , spawn "steam")
  , ("M-S-s"      , spawn "systemctl suspend")
  , ("M-b"        , sendMessage ToggleStruts)
  , ("M-S-b"      , incSpacing mySpacing)
  , ("M-v"        , setSpacing mySpacing)
  , ("M-S-v"      , incSpacing (-mySpacing))
  , ("M-f"        , spawn myBrowser)
  -- control key binds
  , ("C-S-="        , spawn "pactl set-sink-volume 1 +5%")
  , ("C-S--"        , spawn "pactl set-sink-volume 1 -5%")
  --, ("M-a"        , switchProjectPrompt)
  --, ("M-z"        , shiftToProjectPrompt)
  ]

-----------------------------------------------------------------------------}}}
-- managehook                                                                {{{
--------------------------------------------------------------------------------

myManageHook = composeAll
  [ className =? "steam" --> doShift wsGAM
  , className =? "Xmessage" --> doFloat
  , className =? "nautilus" --> doShift wsWRK
  , manageDocks
  , (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)
  ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

-----------------------------------------------------------------------------}}}
-- Startup Hook                                                              {{{
--------------------------------------------------------------------------------
--TODO: check for dual monitor at startup -> xrandr?
--TODO: place trayer, dropbox and nm-applet into xinitrc? or maybe add to systemctl
myStartupHook = do
  --spawn "xrandr --output eDP1 --left-of HDMI1"
  spawn "feh --bg-fill ~/.config/wall.png"
  spawn "xcompmgr -c"
  --spawn "xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'"

  -- Set capslock to escape
  spawn "setxkbmap -option ctrl:nocaps"
  -- in combination with another key capslock becomes ctrl
  spawn "xcape -e 'Control_L=Escape'"

  spawn "~/dotfiles/scripts/extrasuperkeys"

  -- level 3 is selected by the backslash key
  --spawn "setxkbmap -option lv3:bksl_switch"
  -- compose can then be engaged with backslash windows
  --spawn "setxkbmap -option compose:lwin-altgr"


  -- re-enable backslash for typing
  --spawn k

  spawn "trayer --edge top --align right --SetPartialStrut true --transparent true --alpha 000 --tint 0x000000 --expand false --heighttype pixel --height 19 --monitor 0 --padding 1 --widthtype percent --width 5"
  spawn "dropbox"
  spawn "nm-applet"

-- dual monitor? --> xrandr --output <DP-1> --left-of <DP-2> (xrandr -q for the names of DP-1 and DP-2)
