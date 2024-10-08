module Dzen where


import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (hPutStrLn)
import XMonad.Util.WorkspaceCompare (mkWsSort, getSortByIndex)


font = "'FiraCode Nerd Font-11:style=Retina'"


dzenCommand :: String -> String
dzenCommand "anaxagoras" =
  "killall dzen2;" ++
  "dzen2" ++
  " -dock" ++
  " -p" ++
  " -xs 1" ++
  " -ta r" ++
  " -tw 2560" ++
  " -fn " ++ font ++
  " -fg '#ffffff'" ++
  " -bg '#000000'" ++
  " -e 'onStart=lower'"
dzenCommand "heraklit" =
  "killall dzen2;" ++
  "dzen2" ++
  " -dock" ++
  " -p" ++
  " -xs 2" ++
  " -ta r" ++
  " -tw 1920" ++
  " -fn " ++ font ++
  " -fg '#ffffff'" ++
  " -bg '#000000'" ++
  " -e 'onStart=lower'"
dzenCommand "sokrates" =
  "killall dzen2;" ++
  "dzen2" ++
  " -dock" ++
  " -p" ++
  " -xs 2" ++
  " -ta r" ++
  " -tw 2560" ++
  " -fn " ++ font ++
  " -fg '#ffffff'" ++
  " -bg '#000000'" ++
  " -e 'onStart=lower'"


dzenPP' handle = def
  { ppOutput          = hPutStrLn handle
  , ppCurrent         = dzenColor "white" ""
  , ppVisible         = dzenColor "#729fcf" "" -- tango blue
  , ppHidden          = dzenColor "#555753" "" -- tango lightblack
  , ppHiddenNoWindows = const ""
  , ppUrgent          = dzenColor "#ef2929" "" -- tango lightred
  , ppSep             = "    "
  , ppWsSep           = "  "
  , ppTitle           = shorten 100
  , ppLayout          = dzenColor "white" "" . icon
  , ppOrder           = reverse
  , ppSort            = getSortByIndex
  }
  where
    icon layout = case layout of
      "News" -> "^i(.xmonad/icons/grid.xbm)"
      "Chat" -> "^i(.xmonad/icons/grid.xbm)"
      "Dish" -> "^i(.xmonad/icons/layout_dishes.xbm)"
      "Full" -> "^i(.xmonad/icons/layout_full.xbm)"
      "Horizontal" -> "^i(.xmonad/icons/layout_mirror_tall.xbm)"
      "Vertical" -> "^i(.xmonad/icons/layout_tall.xbm)"
      other -> other
