import Data.Monoid

import System.Posix.Unistd

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run


import BorderColors
import KeyBindings
import Programs
import StartUp
import Windows
import Workspaces
import Dzen


-- focus follows the mouse pointer?
focusFollowsMouse' = False


-- event handling
eventHook' = mempty <+> fullscreenEventHook


-- status bars and logging
logHook' handle = fadeInactiveLogHook fadeAmount >> dynamicLogWithPP (dzenPP' handle)
    where
        fadeAmount = 0.7


main = do
    host <- fmap nodeName getSystemID
    dzenHandle <- spawnPipe dzenCommand
    xmonad
        . ewmh
        . withUrgencyHook NoUrgencyHook
        $ defaultConfig
            { terminal           = terminalName
            , focusFollowsMouse  = focusFollowsMouse'
            , modMask            = winMask
            , workspaces         = workspaces'
            , normalBorderColor  = normalBorderColor'
            , focusedBorderColor = focusedBorderColor'
            , keys               = keys' host
            , mouseBindings      = mouseBindings'
            , handleEventHook    = eventHook'
            , logHook            = logHook' dzenHandle
            , startupHook        = startupHook' host
            , manageHook         = manageHook'
            , layoutHook         = layoutHook'
            }
