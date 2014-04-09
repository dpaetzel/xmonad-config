module Workspaces where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

-- workspaces
workspaces' =
    [ "email"
    , "web"
    , "workspace"
    , "3"
    , "4"
    , "5"
    , "vnc"
    , "media"
    , "chat"
    , "top"
    , "trash"
    , "terminal"]


-- workspace layouts
layoutHook' =
    avoidStruts  $
    smartBorders $
    onWorkspace "email"    Full $
    onWorkspace "web"      (Full ||| tiled halfs ||| Mirror (tiled halfs)) $
    -- onWorkspace "vnc"      Full $
    onWorkspace "chat"     Grid $
    onWorkspace "top"      Full $
    onWorkspace "trash"    (Grid ||| Full) $
    onWorkspace "terminal" (Full ||| Grid) $
    tiled thirds ||| tiled halfs ||| Mirror (tiled halfs) ||| noBorders Full

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
