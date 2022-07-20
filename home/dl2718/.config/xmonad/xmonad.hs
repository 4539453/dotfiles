-- Base
import XMonad
import System.Directory
import qualified XMonad.StackSet as W

-- Actions
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

-- Data
import Data.List (elemIndex)
import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops (ewmh) -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
import XMonad.Hooks.Rescreen
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar.PP (filterOutWsPP)
import qualified XMonad.Hooks.StatusBar as StatusBar (withSB, statusBarProp, statusBarPropTo, StatusBarConfig, xmonadDefProp)
import XMonad.Hooks.WorkspaceHistory

-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook)

-- Layouts modifiers
import XMonad.Layout.IndependentScreens (countScreens)
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Reflect
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest

import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Utilities
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

import Theme.Dark


myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty "    -- Sets default terminal

myBrowser :: String
myBrowser = "firefox "       -- Sets browser

myBorderWidth :: Dimension
myBorderWidth = 2            -- Sets border width for windows

myWorkspaces :: [[Char]]
myWorkspaces =  [" dev ", " www ", " doc ", " sys ", " vbox ",
                " chat ", " mus ", " vid ", " gfx "]

myNormColor :: String
myNormColor   = colorBack    -- Border color of normal windows

myFocusColor :: String
myFocusColor  = color05      -- Border color of focused windows

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def
  { fontName            = myFont
  , activeColor         = color06
  , inactiveColor       = color07
  , activeBorderColor   = color06
  , inactiveBorderColor = color08
  , activeTextColor     = color09
  , inactiveTextColor   = color01
  }

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "xbindkeys"
  spawn ("wallpaper " ++ colorScheme ++ "-solid")
  spawn "picom --experimental-backends --config $HOME/.config/picom/picom.conf"
--   spawn ("conky -c $HOME/.config/conky/" ++ colorScheme ++ ".conkyrc")
--   spawn ("tray --edge top --align right --widthtype request --padding 20 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha $(expr 255 - $(cat ~/.config/xmobar/" ++ colorScheme ++ "-main-xmobarrc | grep alpha | cut -d = -f2 | cut -d ' ' -f2)) " ++ colorTop ++ " --height 27 --iconspacing 4")

  setWMName "LG3D"

--------------------------------------------------------------------------------

myAppGrid = [
            ("Xournal++", "xournalpp")
            ]


spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where conf = def {
      gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

--------------------------------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "top" spawnTop findTop manageTop
                -- , NS "calculator" spawnCalc findCalc manageCalc
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.95 -h
                 l = 0.95 -w
    spawnTop  = myTerminal ++ " -t top-scratchpad" ++ " -e btop"
    findTop   = title =? "top-scratchpad"
    manageTop = customFloating $ W.RationalRect l t w h
               where
                 h = 0.67
                 w = 0.56
                 t = 0.98 -h
                 l = 0.98 -w
    -- spawnCalc  = "qalculate-gtk"
    -- findCalc   = className =? "Qalculate-gtk"
    -- manageCalc = customFloating $ W.RationalRect l t w h
    --            where
    --              h = 0.5
    --              w = 0.4
    --              t = 0.75 -h
    --              l = 0.70 -w

--------------------------------------------------------------------------------

-- Custom hook for screen (xrandr) configuration changes.
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Hooks-Rescreen.html
myRandrChangeHook :: X ()
myRandrChangeHook = do
  spawn "layout-auto"

myAfterRescreenHook :: X ()
myAfterRescreenHook = do
  spawn ("wallpaper " ++ colorScheme ++ "-solid")

rescreenCfg = def{
  afterRescreenHook = myAfterRescreenHook,
  randrChangeHook = myRandrChangeHook
}

--------------------------------------------------------------------------------

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
  { swn_font              = "xft:Ubuntu:bold:size=60"
  , swn_fade              = 1.0
  , swn_bgcolor           = "#1c1f24"
  , swn_color             = "#ffffff"
  }

--------------------------------------------------------------------------------

-- Makes setting the spacingRaw simpler to write.
-- The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

--------------------------------------------------------------------------------

-- Defining a bunch of layouts.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
          --  $ smartBorders
           $ windowNavigation
          --  $ addTabs shrinkText myTabTheme
          --  $ subLayout [] (Simplest)
           $ limitWindows 8
           $ mySpacing 4
        --    $ reflectHoriz
           $ ResizableTall 1 (3/100) (1/2) []
mirror   = renamed [Replace "mirror"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 6
           $ mySpacing 4
           $ Mirror (Tall 2 (3/100) (3/4))
tabs     = renamed [Replace "tabs"]
           $ tabbed shrinkText myTabTheme
-- grid     = renamed [Replace "grid"]
--           --  $ smartBorders
--            $ windowNavigation
--           --  $ addTabs shrinkText myTabTheme
--           --  $ subLayout [] (smartBorders Simplest)
--            $ limitWindows 12
--            $ mySpacing 4
--            $ Grid (16/10)
-- floats   = renamed [Replace "floats"]
--           --  $ smartBorders
--            $ limitWindows 20 simplestFloat

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                --  ||| grid
                                 ||| mirror
                                 ||| noBorders tabs

--------------------------------------------------------------------------------

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
    -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
    -- xprop | grep WM_CLASS
    [ checkDialog                    --> doCenterFloat
    , className =? "confirm"         --> doFloat
    , className =? "file_progress"   --> doFloat
    , className =? "dialog"          --> doFloat
    , className =? "download"        --> doFloat
    , className =? "error"           --> doFloat
    , className =? "notification"    --> doFloat
    , className =? "pinentry-gtk-2"  --> doFloat
    , className =? "splash"          --> doFloat
    , className =? "toolbar"         --> doFloat
    , className =? "Gcr-prompter"    --> doFloat
    , className =? "Yad"             --> doFloat


    , className =? "copyq"           --> doFloat
    , className =? "flameshot"       --> doFloat
    , className =? "pentablet"       --> doCenterFloat
    , className =? "zoom"            --> doFloat
    , appName   =? "peek"            --> doFloat
    -- , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
    , title =? "Picture-in-Picture" --> doFloat

    -- , (className =? "chromium" <&&> resource =? "Dialog") --> doFloat  -- Float Chromium Dialog

    , isFullscreen -->  doFullFloat

    -- Shift apps to defined workspaces.
    -- , className =? "Code"                     --> doShift " dev "
    , title     =? "Open Folder"              --> doCenterFloat

    -- , title =? "Mozilla Firefox"              --> doShift " www "

    , className =? "Xournalpp"                --> doShift " doc "
    , className =? "Notion"                   --> doShift " doc "
    , className =? "notion-app-enhanced"      --> doShift " doc "
    , className =? "okular"                   --> doShift " doc "

    , title =? "Oracle VM VirtualBox Manager" --> doFloat
    , className =? "VirtualBox Manager"       --> doShift " vbox "

    -- , className =? "TelegramDesktop"          --> doShift " chat "
    , className =? "Thunderbird"              --> doShift " chat "
    , className =? "discord"                  --> doShift " chat "
    , className =? "zoom"                     --> doShift " chat "

    , className =? "vlc"                      --> doShift " vid "
    , className =? "obs"                      --> doShift " vid "

    , className =? "Gimp-2.10"                --> doFloat
    , className =? "Gimp-2.10"                --> doShift " gfx "
    , className =? "krita"                    --> doShift " gfx "
    ] <+> namedScratchpadManageHook myScratchPads

-- ----------
-- START_KEYS
-- ----------
myKeys :: [(String, X ())]
myKeys =
-- Keys aliases:
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Util-EZConfig.html
-- KB_GROUP Xmonad
  [
    -- KB_GROUP Run Prompt
      ("M-p", spawnRofi "-show drun -show-icons") -- Spawn rofi drun
    , ("M-o", spawnRofi "-show window -show-icons") -- Spawn rofi run

    -- KB_GROUP Useful programs to have a keybinding for launch
    , ("M-<Return>", spawn (myTerminal))
    , ("M-S-l", spawn "xscreensaver-command -lock")
    , ("M-b", spawn (myBrowser))

    -- KB_GROUP Kill windows
    , ("M-S-c", kill1)     -- Kill the currently focused client
    , ("M-S-a", killAll)   -- Kill all windows on current workspace

    -- KB_GROUP Floating windows
    -- , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
    , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
    , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Strachpads
    ,("M-a", namedScratchpadAction myScratchPads "terminal")
    ,("M-s", namedScratchpadAction myScratchPads "top")

    -- KB_GROUP Grid Select (CTR-g followed by a key)
    , ("C-g g", spawnSelected' myAppGrid)           -- grid select favorite apps
    , ("C-g r", spawn (myTerminal ++ "-c lf"))
    , ("C-g f", spawn "pcmanfm")
    , ("C-g b", spawn "obsidian")
    , ("C-g c", spawn "code")
    , ("C-g t", spawn "telegram-desktop")


    -- KB_GROUP Windows navigation
    , ("M-m", windows W.focusMaster)  -- Move focus to the master window
    , ("M-j", windows W.focusDown)    -- Move focus to the next window
    , ("M-k", windows W.focusUp)      -- Move focus to the prev window
    , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
    , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
    , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
    , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
    , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
    , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack


    -- KB_GROUP Layouts
    , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
    , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- KB_GROUP Window resizing
    , ("M-M1-h", sendMessage Shrink)                -- Shrink horiz window width
    , ("M-M1-l", sendMessage Expand)                -- Expand horiz window width
    , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
    , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width

    -- -- KB_GROUP Sublayouts
    -- -- This is used to push windows to tabbed sublayouts, or pull them out of it.
    , ("M-C-h", sendMessage $ pullGroup L)
    , ("M-C-l", sendMessage $ pullGroup R)
    , ("M-C-k", sendMessage $ pullGroup U)
    , ("M-C-j", sendMessage $ pullGroup D)
    , ("M-C-m", withFocused (sendMessage . MergeAll))
    , ("M-C-u", withFocused (sendMessage . UnMerge))
    , ("M-C-/", withFocused (sendMessage . UnMergeAll))
    , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
    , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- KB_GROUP Clipboard
    , ("M-v", spawn ("copyq show"))

    -- KB_GROUP PrtSc
    , ("S-<Print>", spawn ("flameshot full -c -p $HOME/Pictures"))
    , ("<Print>", spawn ("flameshot gui"))

    -- KB_GROUP Fn Keys
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 1%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 1%+")
    -- , ("<XF86Display>", spawn "")
  ]

-- --------
-- END_KEYS
-- --------

main :: IO ()
main = do
  xmonad $
    docks $
    rescreenHook rescreenCfg $
    -- ewmhFullscreen $
    ewmh $
    def
      {
        -- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Layout-Fullscreen.html
        handleEventHook = fullscreenEventHook
      , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
      , manageHook         = myManageHook <+> manageDocks <+> fullscreenManageHook
      , modMask            = myModMask
      , terminal           = myTerminal
      , startupHook        = myStartupHook
      , workspaces         = myWorkspaces
      , borderWidth        = myBorderWidth
      , normalBorderColor  = myNormColor
      , focusedBorderColor = myFocusColor
      } `additionalKeysP` myKeys

-- ------------
-- My functions
-- ------------

-- Get focused screeen to spawm Rofi
reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

physicalScreens :: X [Maybe ScreenId]
physicalScreens = withWindowSet $ \windowSet -> do
  let numScreens = length $ W.screens windowSet
  let reverseDisplayIdList = reverseList [0..numScreens-1]
  mapM (getScreen def . P) reverseDisplayIdList
  -- mapM (\s -> getScreen def (P s)) [0..numScreens] -- #feature

-- If this function seems weird, it's because it's intended to be dual to
--   getScreen :: PhysicalScreen -> X (Maybe ScreenId)
-- from XMonad.Actions.PhysicalScreens.
-- See: https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Actions-PhysicalScreens.html
getPhysicalScreen :: ScreenId -> X (Maybe PhysicalScreen)
getPhysicalScreen sid = do
  pscreens <- physicalScreens
  return $ (Just sid) `elemIndex` pscreens >>= \s -> Just (P s)

rofi :: X String
rofi = withWindowSet $ \windowSet -> do
  let sid = W.screen (W.current windowSet)
  pscreen <- getPhysicalScreen sid
  return $ case pscreen of
            Just (P s) -> "rofi -m " ++ (show s)
            otherwise  -> "rofi"

spawnRofi :: String -> X ()
spawnRofi args = do
  cmd <- rofi
  spawn $ cmd ++ " " ++ args

--------------------------------------------------------------------------------

-- https://wiki.haskell.org/Xmonad/Config_archive/Mntnoe%27s_xmonad.hs
-- | Helper to read a property
-- getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

-- | Check if window is DIALOG window
checkDialog :: Query Bool
checkDialog = ask >>= \w -> liftX $ do
                a <- getAtom "_NET_WM_WINDOW_TYPE"
                dialog <- getAtom "_NET_WM_WINDOW_TYPE_DIALOG"
                mbr <- getProp a w
                case mbr of
                  Just [r] -> return $ elem (fromIntegral r) [dialog]
                  _ -> return False
