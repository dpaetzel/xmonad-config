import Data.Monoid

import System.Posix.Unistd

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive


import BorderColors
import KeyBindings
import Programs
import StartUp
import Windows
import Workspaces


-- focus follows the mouse pointer?
focusFollowsMouse' = False


-- event handling
eventHook' = mempty <+> fullscreenEventHook


-- status bars and logging
logHook' = fadeInactiveLogHook fadeAmount

    where
        fadeAmount = 0.7


main = do
    host <- fmap nodeName getSystemID
    xmonad $ ewmh defaultConfig
        { terminal           = terminal'
        , focusFollowsMouse  = focusFollowsMouse'
        , modMask            = winMask
        , workspaces         = workspaces'
        , normalBorderColor  = normalBorderColor'
        , focusedBorderColor = focusedBorderColor'
        , keys               = keys' host
        , mouseBindings      = mouseBindings'
        , handleEventHook    = eventHook'
        , logHook            = logHook'
        , startupHook        = startupHook' host
        , manageHook         = manageHook'
        , layoutHook         = layoutHook'
        }
