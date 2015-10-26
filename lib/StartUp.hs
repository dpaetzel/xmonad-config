module StartUp where


import Control.Concurrent
import Control.Monad (when)
import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce


import Programs
import ScreenSetup


-- startup
startupHook' :: String -> X ()
startupHook' host = case host of

    -- {{{ anaxagoras specific
    "anaxagoras" -> do

        -- spawnOnceSleep 10 "pidgin"
        spawnOnceSleep 10 "skype"
        -- TODO wrote "weechat" twice
        onceInTerminalWithNameSleep 10 "ircClient" "weechat"
        home "Bin/launchers/wa" >>= spawnOnceSleep 10

        defaultStartupHook
    -- }}}

    -- {{{ heraklit specific
    "heraklit" -> do

        -- screen configuration
        xfork $ screenSetup host

        defaultStartupHook
    -- }}}

    -- {{{ default
    _ -> defaultStartupHook

    where
        defaultStartupHook :: X ()
        defaultStartupHook = do

            -- don't know why i do this
            setWMName "LG3D"

            -- look and feel
            home "Bin/bg-set" >>= spawn
            setDefaultCursor xC_left_ptr
            spawnOnce "dunst -print >> ~/.dunst.log"
            spawnOnce "clipmenud"
            spawnOnce "xcompmgr"
            spawnOnce "xscreensaver -no-splash"
            -- spawnOnce "xmodmap ~/.Xmodmap"
            spawnOnce "setxkbmap -layout de,de -model pc105 -variant neo,basic -option grp:sclk_toggle,grp_led:scroll"
            spawnOnce "fcitx"
            spawnOnce "unclutter -idle 5 -root"
            spawnOnce "xflux -l 48.3 -g 10.9 -k 4000"
            spawnOnce "xset -b"
            spawnOnce "nm-applet"
            spawnOnceSleep 2 "conky"

            -- start applications
            withTerminalWithName spawnOnce "offlineimap" "offlineimap"
            withTerminalWithName spawnOnce "mailClient" "mutt"
            spawnOnce "firefox"
            onceInTerminalWithNameSleep 5 "htop" "htop -u david"
            spawnOnceSleep 10 "telegram"
    -- }}}


-- vim: foldmethod=marker:
