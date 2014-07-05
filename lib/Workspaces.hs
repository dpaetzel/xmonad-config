module Workspaces where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

-- workspaces
workspaces' =
    [ "`:email"
    , "1:web"
    , "2:workspace"
    , "3"
    , "4"
    , "5"
    , "6"
    , "7:media"
    , "8:chat"
    , "9:top"
    , "0:trash"
    , "tab:terminal"]


-- workspace layouts
layoutHook' =
    avoidStruts  $
    smartBorders $
    onWorkspace "`:email"    Full $
    onWorkspace "1:web"      (Full ||| tiled halfs ||| Mirror (tiled halfs)) $
    onWorkspace "8:chat"     Grid $
    onWorkspace "9:top"      Full $
    onWorkspace "0:trash"    (Grid ||| Full) $
    onWorkspace "tab:terminal" (Full ||| Grid) $
    tiled thirds ||| tiled halfs ||| Mirror (tiled halfs) ||| noBorders Full ||| Circle

    where
        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta
        -- The default number of windows in the master pane
        nmaster = 1
        -- Proportion of screen occupied by master pane
        thirds = 2/3
        halfs  = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
