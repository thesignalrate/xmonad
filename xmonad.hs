import XMonad

-- System
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)

-- Operations
import XMonad.Operations (sendMessage)

-- Hooks
import XMonad.Hooks.ManageDocks --(avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))

-- Layouts
import XMonad.Layout
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.CenteredMaster
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.Spacing

-- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.WithAll (sinkAll, killAll)

-- Utilities
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce 
import XMonad.Util.EZConfig

{-
# My configurations
# Some of the configurations is set before passing them to the proper locations. This is done to reuse if needed.
# myModKey -> This is the default mod key. Default value is mod4Mask which is the super key.
# myTerminal :: String: Default terminal, all the operations executed via this terminal.
# myWorkspaces :: [String, ...] -> Default workspaces 
-}

myModKey = mod4Mask
myTerminal = "alacritty"
myWorkspaces = ["dev", "www", "net", "gfx", "media", "tmp"]
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
              $ toggleLayouts Full
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
        , ("M-w", kill1)                           -- Kill the currently focused client
        , ("M-S-a", killAll)                         -- Kill all windows on current workspace

    -- Layouts
        , ("M-S-<Space>", sendMessage ToggleStruts)
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

