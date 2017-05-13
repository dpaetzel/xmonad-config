module Windows where


import XMonad
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers (doCenterFloat, doFullFloat, isFullscreen)


import Applications (signalAppID)


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
        , anyQuery "stalonetray"
        ]
    , these doFloat
        [ title     =? "Firefox Preferences"
        , title     =? "Tab Mix Plus Options"
        , title     =? "FoxyProxy Standard"
        , className =? "scalafx.application.AppHelper"
        , className =? "sun-awt-X11-XDialogPeer"
        , title     =? "Volume Control"
        , className =? "Nm-openconnect-auth-dialog"
        , title     =? "Wireshark"
        -- TODO check whether these are needed/have any merit
        -- , stringProperty "WM_WINDOW_ROLE" =? "app"
        -- , stringProperty "_NET_WM_WINDOW_TYPE" =? "_NET_WM_WINDOW_TYPE_DIALOG"
        ]
    , these doCenterFloat
        [ stringProperty "WM_WINDOW_ROLE" =? "gimp-toolbox-color-dialog"
        , title =? "Wayfinder"
        ]
    , these (doShift "browser")
        [ className =? "chromium"
        , className =? "chromium-browser"
        ]
    , these (doShift "chat")
        [ title     =? "ircClient"
        , title     =? "Whatsie"
        , title     =? "Telegram"
        , className =? "Skype"
        , className =? "Pidgin"
        , className =? "telegram-desktop"
        , className =? "Gajim"
        -- , title     =? "Hangouts" -- not yet working
        -- , className =? ("crx_" ++ telegramAppID) -- not yet working
        -- , className =? ("crx_" ++ signalAppID) -- not yet working
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
