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
import XMonad.Util.EZConfig         ( additionalKeysP )

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

import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad . ewmh . withUrgencyHook NoUrgencyHook .
    withNavigation2DConfig defaultNavigation2DConfig $
      myConfig
        { logHook = dynamicLogWithPP (myLogHook dbus)
        , startupHook = myStartupHook
        } `additionalKeysP` myAdditionalKeys


myStartupHook = do
  setWMName "LG3D"
  spawn "$HOME/dotfiles/.config/polybar/launch2.sh"
  spawn "dropbox"
  spawn "nm-applet"

-- Variables
--------------------------------------------------------------------------------

myTerminal    = "tilix"
myModMask     = mod4Mask
myBorderWidth = 1
myLayout      = Tall 1 (3/100) (1/2)
myBrowser     = "firefox"

mySpacing :: Int
mySpacing     = 5
noSpacing :: Int
noSpacing     = 0

modMask' :: KeyMask
modMask' = mod4Mask

delta :: Rational
delta = 3 / 100

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

myIM :: LayoutClass l a => l a -> ModifiedLayout AddRoster l a
myIM = withIM (1 % 4) (ClassName "TelegramDesktop")

myLayouts = renamed [CutWordsLeft 2] $ spacing mySpacing $ renamed [CutWordsLeft 1] .
    avoidStruts . minimize . B.boringWindows $
    smartBorders
        ( aTiled
        ||| aFullscreen
        ||| aThreeColMid
        )
  where
    aFullscreen = renamed [Replace "Full"] $ noBorders Full
    aTiled = renamed [Replace "Main"] $ myIM $ Tall 1 (3/100) (1/2)
    aThreeColMid = renamed [Replace "3Col"] $ ThreeColMid 1 (3/100) (1/2)

wsGEN = "\xf269"
wsWRK = "\xf02d"
wsSYS = "\xf300"
wsMED = "\xf001"
wsTMP = "\xf2db"
wsGAM = "\xf11b"

workspaces' :: [String]
workspaces' = [wsGEN, wsWRK, wsSYS, wsMED, wsTMP, wsGAM, "7", "8", "9"]

myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , className =? "Gimp"             --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , className =? "feh"              --> doFloat
    , className =? "Gpick"            --> doFloat
    , role =? "pop-up"                --> doFloat ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

myManageHook' = composeOne [ isFullscreen -?> doFullFloat ]

myLogHook :: D.Client -> PP
myLogHook dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
    , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = shorten 25
    }

--TODO: addName?
--TODO: audio function keys
--TODO: move some programs automatically to workspaces
--TODO: split keys in different functionality (system, media, launchers)
myAdditionalKeys =
  [ ("M-z"        , spawn "tilix -e calcurse")
  , ("M-S-z"      , spawn "~/dotfiles/scripts/lockscreen.sh")
  , ("M-s"        , spawn "steam")
  , ("M-S-s"      , spawn "systemctl suspend")
  , ("M-b"        , sendMessage ToggleStruts)
  , ("M-S-b"      , incSpacing mySpacing)
  , ("M-v"        , setSpacing mySpacing)
  , ("M-S-v"      , incSpacing (-mySpacing))
  , ("M-f"        , spawn myBrowser)
  , ("M-g"        , spawn myTerminal)
  , ("M-S-g"      , spawn "tilix -e vim ~/Documents/studie/master/afstudeeropdracht/notes/general.md")
  -- control key binds
  , ("C-S-="        , spawn "pactl set-sink-volume 1 +5%")
  , ("C-S--"        , spawn "pactl set-sink-volume 1 -5%")
  --, ("M-a"        , switchProjectPrompt)
  --, ("M-z"        , shiftToProjectPrompt)
  ]

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

myConfig = def
    { terminal = "tilix"
    , layoutHook = myLayouts
    , manageHook = placeHook (smart (0.5, 0.5))
                    <+> manageDocks
                    <+> myManageHook
                    <+> myManageHook'
                    <+> manageHook def
    , handleEventHook = docksEventHook <+> minimizeEventHook <+> fullscreenEventHook
    -- Don't be stupid with focus
    , focusFollowsMouse = False
    , clickJustFocuses = False
    , borderWidth = myBorderWidth
    , normalBorderColor = gray
    , focusedBorderColor = pur2
    , workspaces = workspaces'
, modMask = modMask' }
