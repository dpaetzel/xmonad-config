module Workspaces where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect

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
    , "browser"
    , "terminal"
    , "editor"]


-- workspace layouts
layoutHook' =
    avoidStruts  $
    smartBorders $
    onWorkspace "0:email"      (tiled halfs ||| noBorders Full) $
    onWorkspace "1:web"        (noBorders Full) $
    onWorkspace "8:chat"       im $
    onWorkspace "9:top"        (noBorders Full) $
    onWorkspace "10:trash"     (Grid ||| Full) $
    onWorkspace "browser"      ((tiled thirds) ||| tiled halfs ||| noBorders Full ||| Mirror (tiled halfs)) $
    onWorkspace "terminal"     (noBorders Full) $
    onWorkspace "editor"       (noBorders Full ||| tiled halfs) $
    tiled halfs ||| Mirror (tiled halfs) ||| noBorders Full ||| Circle

    where
        -- Default tiling algorithm partitions the screen into two panes
        tiled = reflectHoriz . Tall nmaster delta
        -- The default number of windows in the master pane
        nmaster = 1
        -- Proportion of screen occupied by master pane
        halfs  = 1/2
        -- Proportion of screen occupied by master pane
        thirds  = 3/5
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Instant Messaging layout
        im = withIM (1/8) (And (ClassName "Skype") (Not $ Role "ConversationsWindow")) $ tiled halfs
