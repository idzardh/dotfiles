-- IMPORT                                                                    {{{
--------------------------------------------------------------------------------
import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.Submap

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Minimize
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName

import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions

import XMonad.Layout.IM
import XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

import qualified XMonad.Layout.BoringWindows as B
import qualified DBus as D
import qualified DBus.Client as D

import System.Exit
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio ((%))

import System.IO (hClose)

import qualified Codec.Binary.UTF8.String as UTF8

-----------------------------------------------------------------------------}}}
-- MAIN                                                                      {{{
--------------------------------------------------------------------------------
--TODO: move some programs automatically to workspaces
main :: IO ()
main = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad
    -- $ dynamicProjects projects
    $ withNavigation2DConfig defaultNavigation2DConfig
    $ withUrgencyHook NoUrgencyHook
    $ ewmh
    $ addDescrKeys ((myModMask, xK_F1), xMessage) myAdditionalKeys
    -- $ addDescrKeys ((myModMask, xK_F1), showKeybindings) myAdditionalKeys
    $ myConfig { logHook = dynamicLogWithPP (myLogHook dbus) }

-----------------------------------------------------------------------------}}}
-- GLOBAL VARIABLES                                                          {{{
--------------------------------------------------------------------------------
-- General config

myTerminal    = "tilix"
myModMask     = mod4Mask
myBorderWidth = 1
myBrowser     = "firefox"
mySpacing :: Int
mySpacing     = 5
noSpacing :: Int
noSpacing     = 0

-- Colours
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"

pur2      = "#5b51c9"

-----------------------------------------------------------------------------}}}
-- LAYOUT                                                                    {{{
--------------------------------------------------------------------------------
myLayouts = renamed [CutWordsLeft 2] $ spacing mySpacing $ renamed [CutWordsLeft 1] .
    avoidStruts . minimize . B.boringWindows $
    smartBorders
        ( aTiled
        ||| aFullscreen
        ||| aThreeColMid
        )
  where
    aFullscreen = renamed [Replace "Full"] $ noBorders Full
    aTiled = renamed [Replace "Main"] $ Tall 1 (3/100) (1/2)
    aThreeColMid = renamed [Replace "3Col"] $ ThreeColMid 1 (3/100) (1/2)

-----------------------------------------------------------------------------}}}
-- WORKSPACES                                                                {{{
--------------------------------------------------------------------------------
wsGEN = "\xf269"
wsWRK = "\xf02d"
wsSYS = "\xf300"
wsMED = "\xf001"
wsTMP = "\xf2db"
wsGAM = "\xf11b"

workspaces' :: [String]
workspaces' = [wsGEN, wsWRK, wsSYS, wsMED, wsTMP, wsGAM, "7", "8", "9"]

-----------------------------------------------------------------------------}}}
-- KEYBINDINGS                                                               {{{
--------------------------------------------------------------------------------
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe "zenity --text-info --font=adobe courier"
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()

myAdditionalKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
  myProgramKeys ++ myWindowManagerKeys ++ myMediaKeys

myProgramKeys =
  [ ("M-z"        , addName "1" $ spawn "tilix -e calcurse")
  , ("M-S-z"      , addName "2" $ spawn "~/dotfiles/scripts/lockscreen.sh")
  , ("M-s"        , addName "3" $ spawn "steam")
  , ("M-S-s"      , addName "4" $ spawn "systemctl suspend")
  , ("M-f"        , addName "5" $ spawn myBrowser)
  , ("M-g"        , addName "6" $ spawn myTerminal)
  , ("M-S-g"      , addName "7" $ spawn "tilix -e vim ~/Documents/studie/master/afstudeeropdracht/notes/general.md")
  ]

myWindowManagerKeys =
  [ ("M-b"        , addName "1" $ sendMessage ToggleStruts)
  , ("M-S-b"      , addName "1" $ incSpacing mySpacing)
  , ("M-v"        , addName "1" $ setSpacing mySpacing)
  , ("M-S-v"      , addName "1" $ incSpacing (-mySpacing))
  --, ("M-a"        , switchProjectPrompt)
  --, ("M-z"        , shiftToProjectPrompt)
  ]

myMediaKeys =
  [ ("<XF86MonBrightnessUp>"   , addName "1" $ spawn "xbacklight -inc 10")
  , ("<XF86MonBrightnessDown>" , addName "1" $ spawn "xbacklight -dec 10")
  -- mpc
  , ("<XF86AudioPrev>"         , addName "1" $ spawn "mpc prev")
  , ("<XF86AudioNext>"         , addName "1" $ spawn "mpc next")
  , ("<XF86AudioPlay>"         , addName "1" $ spawn "mpc toggle")
  -- volume
  , ("<XF86AudioRaiseVolume>"  , addName "1" $ spawn "pactl set-sink-volume 1 +5%")
  , ("<XF86AudioLowerVolume>"  , addName "1" $ spawn "pactl set-sink-volume 1 -5%")
  , ("<XF86AudioMute>"         , addName "1" $ spawn "pactl set-sink-mute 1 toggle")
  -- volume: for if meta keys are not available
  , ("C-S-="                   , addName "1" $ spawn "pactl set-sink-volume 1 +5%")
  , ("C-S--"                   , addName "test" $ spawn "pactl set-sink-volume 1 -5%")
  ]

-----------------------------------------------------------------------------}}}
-- MANAGEHOOK                                                                {{{
--------------------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , className =? "feh"              --> doFloat
    , className =? "Gpick"            --> doFloat
    , role      =? "pop-up"           --> doFloat ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myManageHook' = composeOne [ isFullscreen -?> doFullFloat ]

-----------------------------------------------------------------------------}}}
-- LOGHOOK                                                                   {{{
--------------------------------------------------------------------------------
myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
    , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = myAddSpaces 25
    }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

myAddSpaces :: Int -> String -> String
myAddSpaces len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

-----------------------------------------------------------------------------}}}
-- STARTUPHOOK                                                               {{{
--------------------------------------------------------------------------------
myStartupHook = do
  setWMName "LG3D"
  spawn "$HOME/dotfiles/.config/polybar/launch.sh"
  spawn "dropbox"

-----------------------------------------------------------------------------}}}
-- CONFIG                                                                    {{{
--------------------------------------------------------------------------------
myConfig = def
    { terminal            = myTerminal
    , layoutHook          = myLayouts
    , manageHook          = placeHook(smart(0.5, 0.5))
        <+> manageDocks
        <+> myManageHook
        <+> myManageHook'
        <+> manageHook def
    , handleEventHook     = docksEventHook
        <+> minimizeEventHook
        <+> fullscreenEventHook
    , startupHook         = myStartupHook
    , focusFollowsMouse   = False
    , clickJustFocuses    = False
    , borderWidth         = myBorderWidth
    , normalBorderColor   = gray
    , focusedBorderColor  = pur2
    , workspaces          = workspaces'
    , modMask             = myModMask
    }
-----------------------------------------------------------------------------}}}
