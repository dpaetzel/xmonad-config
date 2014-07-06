import Data.Monoid

import System.Posix.Unistd

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare


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
logHook' dzenHandle = fadeInactiveLogHook fadeAmount >> dynamicLogWithPP dzenPP'

    where
        fadeAmount = 0.7
        dzenPP' = defaultPP
            { ppOutput          = hPutStrLn dzenHandle
            -- , ppCurrent         = surroundWith "[ " " ]" . return . head
            -- , ppVisible         = surroundWith "[ " " ] " . return . head
            , ppCurrent         = dzenColor "#729fcf" ""
            , ppVisible         = dzenColor "white" ""
            , ppHidden          = dzenColor "#555753" ""
            , ppHiddenNoWindows = const ""
            , ppUrgent          = dzenColor "#ef2929" ""
            , ppSep             = "  |  "
            , ppWsSep           = "  "
            , ppTitle           = shorten 50
            -- , ppLayout          = surroundWith "[ " " ]"
            , ppLayout          = dzenColor "white" "" . icon
            , ppOrder           = reverse
            -- , ppSort            = mkWsSort getXineramaPhysicalWsCompare
            -- , ppExtras          =
            }
        surroundWith l r s = l ++ s ++ r
        icon layout = case layout of
            "Tall" -> "^i(/home/david/.xmonad/icons/layout_tall.xbm)"
            "Mirror Tall" -> "^i(/home/david/.xmonad/icons/layout_mirror_tall.xbm)"
            "Full" -> "^i(/home/david/.xmonad/icons/layout_full.xbm)"
            "Grid" -> "^i(/home/david/.xmonad/icons/grid.xbm)"
            "Circle" -> "^i(/home/david/.xmonad/icons/empty.xbm)"
            other -> other


main = do
    host <- fmap nodeName getSystemID
    -- xmobarHandle <- spawnPipe "xmobar -b"
    dzenHandle <- spawnPipe "dzen2 -p -xs 1 -ta r -fn Inconsolata-14:normal -fg '#ffffff' -bg '#000000' -e 'onStart=lower'"
    xmonad
        . ewmh
        . withUrgencyHook NoUrgencyHook
        $ defaultConfig
            { terminal           = terminal'
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
