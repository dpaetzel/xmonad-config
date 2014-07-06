module Windows where


import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers


-- window rules
manageHook' = (composeAll . concat $
    [ [anyQuery x --> doIgnore               | x <- ignore]
    , [anyQuery x --> doShift "terminal"     | x <- terminal]
    , [anyQuery x --> doShift "0:email"      | x <- email]
    , [anyQuery x --> doShift "1:web"        | x <- web]
    , [anyQuery x --> doShift "2"            | x <- dev]
    , [anyQuery x --> doShift "3"            | x <- java]
    -- , [anyQuery x --> doShift "vnc"       | x <- vnc]
    , [anyQuery x --> doShift "7:media"      | x <- media]
    , [anyQuery x --> doShift "10:trash"     | x <- trash]
    , [anyQuery x --> doCenterFloat          | x <- center]
    , [anyQuery x --> doFloat                | x <- float' ++ java]

    , [x --> doFloat          | x <- splash ++ float]
    , [x --> doShift "9:top"  | x <- top]
    , [x --> doShift "8:chat" | x <- chat]

    , [manageDocks]
    ])
    <+> manageSpawn
    <+> manageDocks
    <+> manageHook defaultConfig

    where
        email    = ["mailClient", "Mail", "Thunderbird", "mutt"]
        ignore   = ["desktop_window", "desktop", "notify-osd", "trayer", "stalonetray"]
        web      = ["Firefox", "Chromium", "Google-chrome", "Chromium-browser"]
        dev      = ["Eclipse"]
        float'   = ["File Transfers", "java", "Steam", "dota_linux", "mandelbrot"]
        center   = ["MPlayer", "Plugin-container"]
        media    = ["youtube-viewer", "musicPlayer", "spotify", "plugin-container"]
        vnc      = ["vncviewer", "vinagre"]
        java     = ["Intelligent SpeedMeter", "sun-awt-X11-XFramePeer", "Main", "MeinProgramm",
                    "openDLX-main-OpenDLXSimulatorMain", "u11-gui-Main", "java"]
        trash    = ["offlineimap", "vino"]
        terminal = ["terminal"]


        float  = [title =? "Firefox Preferences"]
        splash = [title =? "Wireshark"]
        top    =
            [ title =? "htop"
            , title =? "powertop"]
        chat   =
            [ title =? "ircClient"
            , title =? "Skype"
            , title =? "Pidgin"
            , title =? "Gajim"]

        anyQuery x = fmap or . mapM (=? x) $ [resource, className, title]
