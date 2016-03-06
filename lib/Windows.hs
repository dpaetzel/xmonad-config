module Windows where

import XMonad
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import XMonad.Util.WindowPropertiesRE ((~?))


-- window rules
manageHook' :: ManageHook
manageHook' = manageSpawn
    <+> (composeAll . concat $
    -- TODO all ignores still needed?
    [ these doIgnore
        [ anyQuery "desktop_window"
        , anyQuery "desktop"
        , anyQuery "notify-osd"
        , anyQuery "trayer"
        , anyQuery "stalonetray"]
    , these doFloat
        [ title     =? "Firefox Preferences"
        , title     =? "Tab Mix Plus Options"
        , title     =? "FoxyProxy Standard"
        , className =? "scalafx.application.AppHelper"
        , className =? "sun-awt-X11-XDialogPeer"
        , title     =? "Volume Control"
        , className =? "Nm-openconnect-auth-dialog"
        , title     =? "Wireshark"
        ]
    , these (doShift "news")
        [ title =? "mail"
        , title =? "twitter"
        , title =? "Thunderbird"
        ]
    , these (doShift "browser")
        [ className =? "chromium"
        , className =? "chromium-browser"
        ]
    , these (doShift "7:media")
        [ className =? "Spotify"
        , title     =? "youtube-viewer"
        ]
    , these (doShift "8:chat")
        [ title     =? "ircClient"
        , title     ~? ".*WhatsApp.*"
        , className =? "Skype"
        , className =? "Pidgin"
        , className =? "Telegram"
        , className =? "Gajim"
        , title     =? "Hangouts" -- not yet working
        ]
    , these (doShift "9:top")
        [ title =? "htop"
        , title =? "powertop"
        ]
    , these (doShift "editor")
        [ className =? "Emacs"
        ]
    , [isFullscreen --> doFullFloat]
    ])
    <+> manageDocks
    <+> manageHook def

    where
        anyQuery x = fmap or . mapM (=? x) $ [resource, className, title]
        these doAction = map (flip (-->) doAction)
