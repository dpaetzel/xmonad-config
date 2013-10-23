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
    onWorkspace "web"      Full $
    onWorkspace "vnc"      Full $
    onWorkspace "chat"     Grid $
    onWorkspace "top"      Full $
    onWorkspace "trash"    Grid $
    onWorkspace "terminal" Full $
    tiled ||| Mirror tiled ||| noBorders Full

    where
        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
