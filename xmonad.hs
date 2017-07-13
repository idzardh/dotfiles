import XMonad                        hiding ( (|||) )
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ThreeColumns
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

myLayouts = spacing mySpacing $ myLayout ||| ThreeColMid 1 (3/100) (1/2) ||| ThreeCol 1 (3/100) (1/3) ||| Full

-----------------------------------------------------------------------------}}}
-- Workspaces                                                                {{{
--------------------------------------------------------------------------------

-- fa icons!
wsGEN = "1: <fn=1>\xf269</fn>"
wsWRK = "2: <fn=1>\xf02d</fn>"
wsSYS = "3: <fn=1>\xf300</fn>"
wsMED = "4: <fn=1>\xf001</fn>"
wsTMP = "5: <fn=1>\xf103</fn>"
wsGAM = "6: <fn=1>\xf11b</fn>"

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
myAdditionalKeys =
  [ ("M-z"        , spawn "~/dotfiles/scripts/lockscreen.sh")
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
myStartupHook = do
  --spawn "xrandr --output eDP1 --left-of HDMI1"
  spawn "feh --bg-fill ~/Pictures/awesome.jpg"
  spawn "xcompmgr -c"
  spawn "xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'"
  -- trayer necessary!
  spawn "trayer --edge top --align right --SetPartialStrut true --transparent true --alpha 030 --tint 0x000000 --expand false --heighttype pixel --height 19 --monitor 0 --padding 1 --widthtype percent --width 5"
  spawn "dropbox"
  -- spawn "nm-applet"

-- dual monitor? --> xrandr --output <DP-1> --left-of <DP-2> (xrandr -q for the names of DP-1 and DP-2)

{-{{{
--myLayoutHook = showWorkspaceName -- TODO: find showWorkspaceName
--             $ onWorkspace wsFLOAT floatWorkSpace
--             $ fullscreenFloat
--             $ fullScreenToggle
--             $ fullBarToggle
--             $ mirrorToggle
--             $ reflectToggle
--             $ flex ||| tabs
--  where
--    floatWorkSpace    = simplestFloat
--    fullBarToggle     = mkToggle (single FULLBAR)
--    fullScreenToggle  = mkToggle (single FULL)
--    mirrorToggle      = mkToggle (single MIRROR)
--    reflectToggle     = mkToggle (single REFLECTX)
--    smallMonResWidth  = 1920
--    showWorkspaceName = showName' myShowNameTheme
--
--    named n           = renamed [(XMonad.Layout.Renamed.Replace n)]
--    trimNamed w n     = renamed [(XMonad.Layout.Renamed.CutWordsLeft w)]
--    suffixed n        = renamed [(XMonad.Layout.Renamed.PrependWords n)]
--    trimSuffixed w n  = renamed [(XMonad.Layout.Renamed.CutWordsRight w),
--                                 (XMonad.Layout.Renamed.AppendWords n)]
--
--    addTopBar         = noFrillsDeco shrinkText topBarTheme
--
--    mySpacing         = spacing gap
--    sGap              = quot gap 2
--    myGaps            = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
--    mySmallsGaps      = gaps [(U, sGap),(D, sGap),(L, sGap),(R, sGap)]
--    myBiggap*2s       = gaps [(U, gap*2),(D, gap*2),(L, gap*2),(R, gap*2)]
--
--    -- Tabs layout
--
--    threeCol = named "Unflexed"
--        $ avoidStruts
--        $ addTopBar
--        $ addTabs shrinkText myTabTheme
}}}-}
