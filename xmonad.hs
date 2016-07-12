import Data.Monoid

import System.Posix.Unistd (getSystemID, nodeName)

import XMonad
import XMonad.Actions.DynamicProjects (dynamicProjects)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run (spawnPipe)

import BorderColors
import KeyBindings
import Programs
import Projects
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
        . dynamicProjects projects myXPConfig
        $ def
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
