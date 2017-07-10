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
import XMonad.Hooks.ManageDocks             ( avoidStruts, manageDocks, docksEventHook)
import XMonad.Hooks.DynamicLog

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
      , manageHook         = manageDocks <+> manageHook def
      , borderWidth        = myBorderWidth
      , normalBorderColor  = "#000000"
      , focusedBorderColor = "#2c8fa0"
      , layoutHook         = avoidStruts $ smartBorders $ myLayouts
      --, layoutHook         = avoidStruts $ layoutHook def
      , handleEventHook    = handleEventHook def <+> docksEventHook
      , startupHook        = myStartupHook
      , logHook            = dynamicLogWithPP $ xmobarPP
        { ppOutput    = hPutStrLn xmproc
        , ppCurrent   = xmobarColor "#2c8fa0" "" . wrap "[" "]"
        , ppSep       = " | "
        , ppLayout    = (\x -> case x of
                            "SmartSpacing 5 Tall"       -> " <fn=1>\xf04c</fn> "
                            "SmartSpacing 5 ThreeCol"   -> " <fn=1>\xe0cf</fn> "
                            _             -> x
                        )
        , ppTitle     = xmobarColor "#2c8fa0" "" . shorten 50
        }
      , workspaces         = myWorkspaces
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

-----------------------------------------------------------------------------}}}
-- Layouts                                                                   {{{
--------------------------------------------------------------------------------

myLayouts = smartSpacing mySpacing $ myLayout ||| ThreeColMid 1 (3/100) (1/2) ||| ThreeColMid 1 (3/100) (1/3)

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
myAdditionalKeys =
  [ ("M-f"        , spawn myBrowser) --TODO addName?
  , ("M-s"        , spawn "steam")
  , ("M-S-s"      , spawn "systemctl suspend")
  --, ("M-a"        , switchProjectPrompt)
  --, ("M-z"        , shiftToProjectPrompt)
  ]

-----------------------------------------------------------------------------}}}
-- managehook                                                                {{{
--------------------------------------------------------------------------------

-----------------------------------------------------------------------------}}}
-- Startup Hook                                                              {{{
--------------------------------------------------------------------------------
myStartupHook = do
  spawn "feh --bg-fill ~/Pictures/awesome.jpg"
  spawn "xcompmgr -c"
  spawn "xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'"

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
