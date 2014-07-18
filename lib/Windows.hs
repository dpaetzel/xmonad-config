module Windows where


import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers


-- window rules
manageHook' = (composeAll . concat $
    [ [anyQuery x --> doIgnore               | x <- ignore]
    , [anyQuery x --> doShift "0:email"      | x <- email]
    , [anyQuery x --> doShift "2"            | x <- dev]
    , [anyQuery x --> doShift "3"            | x <- java]
    -- , [anyQuery x --> doShift "vnc"       | x <- vnc]
    , [anyQuery x --> doShift "7:media"      | x <- media]
    , [anyQuery x --> doShift "10:trash"     | x <- trash]
    -- , [anyQuery x --> doCenterFloat          | x <- center]
    , [anyQuery x --> doFloat                | x <- float' ++ java]

    , [x --> doFloat            | x <- splash ++ float]
    , [x --> doShift "1:web"    | x <- web]
    , [x --> doShift "8:chat"   | x <- chat]
    , [x --> doShift "9:top"    | x <- top]
    , [x --> doShift "terminal" | x <- terminal]

    , [isFullscreen --> doFullFloat]
    , [manageDocks]
    ])
    <+> manageSpawn
    <+> manageDocks
    <+> manageHook defaultConfig

    where
        email    = ["mailClient", "Mail", "Thunderbird", "mutt"]
        ignore   = ["desktop_window", "desktop", "notify-osd", "trayer", "stalonetray"]
        -- web'      = ["Firefox", "Chromium", "Google-chrome", "Chromium-browser"]
        dev      = ["Eclipse"]
        float'   = ["File Transfers", "java", "Steam", "dota_linux", "mandelbrot"]
        center   = ["MPlayer", "Plugin-container"]
        media    = ["youtube-viewer", "musicPlayer", "spotify"] --, "plugin-container"]
        vnc      = ["vncviewer", "vinagre"]
        java     = ["Intelligent SpeedMeter", "sun-awt-X11-XFramePeer", "Main", "MeinProgramm",
                    "openDLX-main-OpenDLXSimulatorMain", "u11-gui-Main", "java"]
        trash    = ["offlineimap", "vino"]


        float  = [title =? "Firefox Preferences"]
        splash = [title =? "Wireshark"]
        terminal = [ className =? "terminal"]
        top =
            [ title =? "htop"
            , title =? "powertop"
            ]
        chat =
            [ title     =? "ircClient"
            , className =? "Skype"
            , className =? "Pidgin"
            , className =? "Gajim"
            ]
        web =
            [ className =? "Firefox"
            , className =? "Chromium"
            ]

        anyQuery x = fmap or . mapM (=? x) $ [resource, className, title]
