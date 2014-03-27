module Windows where


import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers


-- window rules
manageHook' = (composeAll . concat $
    [ [anyQuery x --> doIgnore            | x <- ignore]
    , [anyQuery x --> doShift "terminal"  | x <- terminal]
    , [anyQuery x --> doShift "email"     | x <- email]
    , [anyQuery x --> doShift "web"       | x <- web]
    , [anyQuery x --> doShift "workspace" | x <- dev]
    , [anyQuery x --> doShift "3"         | x <- java]
    , [anyQuery x --> doShift "chat"      | x <- chat ++ irc]
    , [anyQuery x --> doShift "top"       | x <- top]
    , [anyQuery x --> doShift "vnc"       | x <- vnc]
    , [anyQuery x --> doShift "media"     | x <- media]
    , [anyQuery x --> doShift "trash"     | x <- trash]
    , [anyQuery x --> doCenterFloat       | x <- center]
    , [anyQuery x --> doFloat             | x <- float ++ java]
    , [manageDocks]
    ])
    <+> manageDocks
    <+> manageHook defaultConfig

    where
        email    = ["mailClient", "Mail", "Thunderbird", "mutt"]
        ignore   = ["desktop_window", "desktop", "notify-osd", "trayer", "stalonetray"]
        chat     = ["Skype", "Pidgin", "Gajim"]
        web      = ["Firefox", "Chromium", "Google-chrome", "Chromium-browser"]
        dev      = ["Eclipse"]
        top      = ["htop", "powertop"]
        irc      = ["weechat"]
        float    = ["File Transfers", "java", "Steam", "dota_linux", "mandelbrot"]
        center   = ["MPlayer", "Plugin-container"]
        media    = ["youtube-viewer", "musicPlayer", "spotify", "plugin-container"]
        vnc      = ["vncviewer"]
        java     = ["Intelligent SpeedMeter", "sun-awt-X11-XFramePeer", "Main", "MeinProgramm",
                    "openDLX-main-OpenDLXSimulatorMain", "u11-gui-Main", "java"]
        trash    = ["offlineimap", "vino"]
        terminal = ["terminal"]

        anyQuery x = fmap or . mapM (=? x) $ [resource, className, title]
