import BorderColors
import Data.Monoid
import Dzen
import KeyBindings
import Programs
import StartUp
import System.Posix.Unistd (getSystemID, nodeName)
import Terminal
import Windows
import Workspaces
import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.ManageDocks (docksEventHook)
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedActions as NA (addDescrKeys', xMessage)
import XMonad.Util.Run (spawnPipe)

eventHook' = mempty <+> docksEventHook <+> fullscreenEventHook

-- status bars and logging
logHook' handle = fadeInactiveLogHook fadeAmount >> dynamicLogWithPP (dzenPP' handle)
  where
    fadeAmount = 0.8

main = do
  host <- fmap nodeName getSystemID
  dzenHandle <- spawnPipe (dzenCommand host)
  xmonad
    . ewmh
    . withUrgencyHook NoUrgencyHook
    . NA.addDescrKeys' ((winMask, xK_udiaeresis), xMessage) (namedKeys host)
    $ def
      { terminal = terminalName,
        focusFollowsMouse = False,
        modMask = winMask,
        workspaces = workspaces',
        normalBorderColor = normalBorderColor',
        focusedBorderColor = focusedBorderColor',
        borderWidth = 2,
        mouseBindings = mouseBindings',
        handleEventHook = eventHook',
        logHook = logHook' dzenHandle,
        startupHook = startupHook' host,
        manageHook = manageHook',
        layoutHook = layoutHook'
      }
