module Windows where


import Text.Regex.Posix
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Scratchpad
import XMonad.Util.WindowPropertiesRE
import qualified XMonad.StackSet as W


-- window rules
manageHook' = manageSpawn
    <+> (composeAll . concat $
    [ [anyQuery x --> doIgnore               | x <- ignore]
    , [anyQuery x --> doShift "2"            | x <- dev]
    -- , [anyQuery x --> doShift "3"            | x <- java]
    -- , [anyQuery x --> doShift "vnc"       | x <- vnc]
    , [anyQuery x --> doShift "7:media"      | x <- media']
    , [anyQuery x --> doShift "10:trash"     | x <- trash]
    , [anyQuery x --> doFloat                | x <- float'] -- ++ java]

    , [x --> doFloat            | x <- splash ++ float]
    , [x --> doCenterFloat      | x <- centerFloat]
    , [x --> doShift "news"     | x <- news]
    , [x --> doShift "1:web"    | x <- web]
    , [x --> doShift "browser"  | x <- browser]
    , [x --> doShift "7:media"  | x <- media]
    , [x --> doShift "8:chat"   | x <- chat]
    , [x --> doShift "9:top"    | x <- top]
    , [x --> doShift "editor"   | x <- editor]

    , [isFullscreen --> doFullFloat]
    , [manageDocks]
    ])
    -- TODO which one
    <+> manageDocks
    -- <+> scratchpadManageHook (W.RationalRect 0 0 1 1)
    <+> manageHook defaultConfig

    where
        ignore   = ["desktop_window", "desktop", "notify-osd", "trayer", "stalonetray"]
        dev      = ["Eclipse"]
        float'   = ["File Transfers", "java", "Steam", "dota_linux", "mandelbrot"]
        media'    = ["youtube-viewer", "musicPlayer"] --, "plugin-container"]
        vnc      = ["vncviewer", "vinagre"]
        java     = ["Intelligent SpeedMeter", "sun-awt-X11-XFramePeer", "Main", "MeinProgramm",
                    "openDLX-main-OpenDLXSimulatorMain", "u11-gui-Main", "java"]
        trash    = ["offlineimap", "vino"]


        float =
            [ title     =? "Firefox Preferences"
            , title     =? "Tab Mix Plus Options"
            , title     =? "FoxyProxy Standard"
            , className =? "scalafx.application.AppHelper"
            , className =? "sun-awt-X11-XDialogPeer"
            , title     =? "Volume Control"
            , className =? "Nm-openconnect-auth-dialog"
            ]
        centerFloat = []
        splash = [title =? "Wireshark"]
        top =
            [ title =? "htop"
            , title =? "powertop"
            ]
        chat =
            [ title     =? "ircClient"
            , title     ~? ".*WhatsApp.*"
            , className =? "Skype"
            , className =? "Pidgin"
            , className =? "Telegram"
            , className =? "Gajim"
            , title     =? "Hangouts" -- not yet working
            ]
        web =
            [ className =? "Firefox"
            , className =? "Vimb"
            ]
        browser =
            [ className =? "chromium"
            , className =? "chromium-browser"
            ]
        editor =
            [ className =? "Emacs"
            ]
        media =
            [ className =? "Spotify"
            ]
        news =
            [ title =? "mail"
            , title =? "Thunderbird"
            ]

        anyQuery x = fmap or . mapM (=? x) $ [resource, className, title]
