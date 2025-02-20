module Workspaces where

import XMonad
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Dishes
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Renamed
import XMonad.Layout.ThreeColumns


-- workspaces
workspaces' :: [String]
workspaces' = generalPurposeWS ++ specialWS


generalPurposeWS =
    [ "1"
    , "2"
    , "3"
    , "4"
    , "5"
    , "6"
    , "7"
    , "8"
    , "9"
    , "0"
    ]


specialWS =
    [ "browser"
    , "chat"
    , "editor"
    , "obsidian"
    , "terminal"
    , "trash"
    ]


-- workspace layouts
layoutHook' =
    avoidStruts  $
    smartBorders $
    onWorkspace "chat"      chatLayout $
    onWorkspace "trash"     dishLayout $
    fullLayout ||| vertical halfs ||| horizontal halfs ||| centered

    where
        -- layout on the chat workspace
        chatLayout = rename "Chat" . withIM (1/8) (And (ClassName "Skype") (Not $ Role "ConversationsWindow")) $ vertical halfs
        -- layout on the trash workspace
        dishLayout = rename "Dish" . limitWindows 7 . Dishes nmaster $ 1/7
        -- fullscreen layout
        fullLayout = rename "Full" $ noBorders Full
        -- horizontal tiled layout
        horizontal = rename "Horizontal" . Mirror . Tall nmaster delta
        -- vertical tiled layout
        vertical = rename "Vertical" . Tall nmaster delta
        -- centered layout for better focus when writing/reading
        centered = ThreeColMid nmaster delta $ 2/3
        -- default number of windows in the master pane
        nmaster = 1
        -- percent of screen to increment by when resizing panes
        delta   = 3/100
        -- proportion of screen occupied by master pane
        halfs  = 1/2
        -- rename a layout
        rename = renamed . return . Replace
