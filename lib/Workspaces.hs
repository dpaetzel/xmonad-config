module Workspaces where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace

-- workspaces
workspaces' =
    [ "0:email"
    , "1:web"
    , "2"
    , "3"
    , "4"
    , "5"
    , "6"
    , "7:media"
    , "8:chat"
    , "9:top"
    , "10:trash"
    , "terminal"
    , "editor"]


-- workspace layouts
layoutHook' =
    avoidStruts  $
    smartBorders $
    onWorkspace "0:email"      (noBorders Full) $
    onWorkspace "1:web"        (noBorders Full ||| tiled halfs ||| Mirror (tiled halfs)) $
    onWorkspace "8:chat"       im $
    onWorkspace "9:top"        (noBorders Full) $  -- ||| tiled halfs ||| Mirror (tiled halfs) ||| Circle) $
    onWorkspace "10:trash"     (Grid ||| Full) $
    onWorkspace "terminal"     (noBorders Full) $
    onWorkspace "editor"       (noBorders Full ||| tiled halfs) $
    tiled halfs ||| Mirror (tiled halfs) ||| noBorders Full ||| Circle

    where
        -- Default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta
        -- The default number of windows in the master pane
        nmaster = 1
        -- Proportion of screen occupied by master pane
        halfs  = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Instant Messaging layout
        im = withIM (1/8) (ClassName "Skype") $ tiled halfs
