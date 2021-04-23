import XMonad

-- System
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)

-- Data
import Data.Monoid


-- Operations
import XMonad.Operations (sendMessage)

-- Hooks
import XMonad.Hooks.ManageDocks --(avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.InsertPosition

-- Layouts
import XMonad.Layout
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.CenteredMaster
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Actions
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies, copyToAll, copyWindow)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.Promote
import XMonad.Actions.WindowGo (runOrRaise)

-- Utilities
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce 
import XMonad.Util.EZConfig

-- Others
import qualified XMonad.StackSet as W

{-
# My configurations
# Some of the configurations is set before passing them to the proper locations. This is done to reuse if needed.
# myModKey -> This is the default mod key. Default value is mod4Mask which is the super key.
# myTerminal :: String: Default terminal, all the operations executed via this terminal.
# myWorkspaces :: [String, ...] -> Default workspaces 
-}

myModKey = mod4Mask
myTerminal = "alacritty"
myEditor = "emacsclient -c -a 'emacs' "
myEmacs = "emacsclient -c -a 'emacs' "
myWorkspaces = ["dev", "www", "dev2", "net", "gfx", "media", "vbox", "tmp"]
myBorderWidth = 3


data MyColors = MyColors { mycNormalBorder :: String
                         , mycFocusedBorder :: String
                         }

myColors = MyColors { mycNormalBorder = "#dddddd"
                    , mycFocusedBorder = "#cc0000"
                    }

myStartupHook :: X ()
myStartupHook = do
          spawnOnce "nitrogen --restore &"
          spawnOnce "picom --experimental-backends --shadow-exclude bounding_shaped --config ~/.config/picom/picom.conf &"
          spawnOnce "nm-applet &"
          spawnOnce "/usr/bin/emacs --daemon &"
          spawnOnce "~/.dotfiles/scripts/k &"
          spawnOnce "sxhkd &"

doCopy :: WorkspaceId -> ManageHook
doCopy m = ask >>= \w -> doF (copyWindow w m)

appendHookCopy [] = doCopy(myWorkspaces !! 1) -- To avoid infinite recursion apply copying to workspace 1 again.
appendHookCopy (w:wm) = doCopy(w) <+> appendHookCopy(wm)
doCopyToAll :: ManageHook
doCopyToAll = appendHookCopy myWorkspaces


myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces and the names would be very long if using clickable workspaces.
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "Gimp"            --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "pinentry-gtk-2"  --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
     --, title =? "Oracle VM VirtualBox Manager"  --> doFloat
     , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
     , className =? "brave-browser"   --> doShift ( myWorkspaces !! 1 )
     , className =? "qutebrowser"     --> doShift ( myWorkspaces !! 1 )
     , title =? "video0 - mpv"        --> insertPosition Below Older <+> doFloat <+> doCopyToAll
     , className =? "mpv"             --> doShift ( myWorkspaces !! 5 )
     , className =? "Gimp"            --> doShift ( myWorkspaces !! 8 )
     , className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 6 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ] -- <+> namedScratchpadManageHook myScratchPads

-- Layouts

spacingModified i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ spacingModified 3
           $ Tall 1 (3/100) (1/2) 

centeredmaster     = centerMaster
           $ renamed [Replace "centered"]
           $ limitWindows 12
           $ spacingModified 3
           $ Tall 1 (3/100) (1/2)

full 	= renamed [Replace "full"]
          $ Full

twopane = renamed [Replace "twopane"]
          $ TwoPane (15/100) (55/100)

mirror = renamed [Replace "mirror"]
         $ Mirror (Tall 1 (10/100) (60/100))

grid = renamed [Replace "grid"]
       $ Grid
           
myLayoutHook =  avoidStruts 
              $ T.toggleLayouts Full
              $ smartBorders
                          (tall
                       ||| centeredmaster
                       ||| full
                       ||| twopane
                       ||| mirror
                       ||| grid
                       ||| simpleTabbed)
              

myKeys :: [(String, X ())]
myKeys =
    -- XMonad
        [ ("M-C-r", spawn "xmonad --recompile")      -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")        -- Restarts xmonad
        , ("M-S-q", io exitSuccess)                  -- Quits xmonad

    -- Open my preferred terminal
        , ("M-<Return>", spawn myTerminal)
    -- Windows
        , ("M-S-c", kill1) 
        , ("M-<Backspace>", killAllOtherCopies <+> kill1)
        , ("M-<Escape>", killAllOtherCopies)
        , ("M-S-a", killAll)                         -- Kill all windows on current workspace
    -- Copy
        , ("M-x c", windows copyToAll)
    -- Camera
        , ("M-c", spawn "toggle_camera")
    -- Layouts
        , ("M-S-<Space>", sendMessage ToggleStruts)

     -- Floating windows
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
        , ("M-d", decWindowSpacing 4)           -- Decrease window spacing
        , ("M-i", incWindowSpacing 4)           -- Increase window spacing
        , ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
        , ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing

      -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-f", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack


        -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Expand vert window width


        -- Controls for mocp music player (SUPER-u followed by a key)
        , ("M-u p", spawn "mocp --play")
        , ("M-u l", spawn "mocp --next")
        , ("M-u h", spawn "mocp --previous")
        , ("M-u <Space>", spawn "mocp --toggle-pause")

        -- Emacs (Super-e followed by a key)
        , ("M-e e", spawn myEmacs)                 -- start emacs
        , ("M-e r", spawn (myEmacs ++ ("--eval '(dashboard-refresh-buffer)'")))   -- emacs dashboard
        , ("M-e b", spawn (myEmacs ++ ("--eval '(ibuffer)'")))   -- list buffers
        , ("M-e d", spawn (myEmacs ++ ("--eval '(dired nil)'"))) -- dired
        , ("M-e m", spawn (myEmacs ++ ("--eval '(mu4e)'")))      -- mu4e email
        , ("M-e s", spawn (myEmacs ++ ("--eval '(eshell)'")))    -- eshell
        , ("M-e o", spawn (myEmacs ++ ("--eval '(progn (switch-to-buffer \"*scratch.org*\") (org-mode))'")))

        -- Multimedia Keys
        , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
        , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
        , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.duckduckgo.com/"])
        , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
        , ("<XF86Eject>", spawn "toggleeject")
        , ("<Print>", spawn "scrotd 0")


        ]
  
main = do
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc" 
    xmonad $ fullscreenSupport $ docks def
       { modMask = myModKey
       , workspaces = myWorkspaces
       , terminal = myTerminal
       , focusFollowsMouse = False
       , normalBorderColor = mycNormalBorder myColors
       , focusedBorderColor = mycFocusedBorder myColors
       , borderWidth = myBorderWidth
       , layoutHook = myLayoutHook
       , startupHook = myStartupHook
       , manageHook = myManageHook
       , logHook = dynamicLogWithPP $ xmobarPP
              -- the following variables beginning with 'pp' are settings for xmobar.
              { ppOutput = hPutStrLn xmproc0
              , ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"           -- Current workspace
              , ppVisible = xmobarColor "#98be65" ""               -- Visible but not current workspace
              , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "" -- Hidden workspaces
              , ppHiddenNoWindows = xmobarColor "#c792ea" ""      -- Hidden workspaces (no windows)
              , ppTitle = xmobarColor "#b3afc2" "" . shorten 60               -- Title of active window
              , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"                    -- Separator character
              , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"            -- Urgent workspace
              , ppExtras  = []                                     -- # of windows current workspace
              , ppLayout = (\l -> "<fn=0><fc=#d8a752>layout: " ++ l ++ "</fc></fn>")             
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]                    -- order of things in xmobar
              }
       } `additionalKeysP` myKeys

