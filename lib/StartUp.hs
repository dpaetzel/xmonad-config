module StartUp where

import XMonad
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.SpawnOnce (spawnOnce)

import Programs


-- startup
startupHook' :: String -> X ()
startupHook' host = case host of

    -- {{{ anaxagoras specific
    "anaxagoras" -> do

        spawnOnceSleep 10 "telegram"
        spawnOnceSleep 10 "skype"
        home "Bin/launchers/wa" >>= spawnOnceSleep 10

        defaultStartupHook
    -- }}}

    -- {{{ heraklit specific
    "heraklit" -> do

        inTerminalWithName "Telegram" "telegram-cli"

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
            spawnOnce "parcellite"
            spawnOnce "xcompmgr"
            spawnOnce "unclutter -idle 5 -root"
            spawnOnce "redshift -l 48.3:10.9 -t 5500:2800"
            spawnOnce "xset -b"
            spawnOnceSleep 2 "conky"

            -- start applications
            withTerminalWithName spawnOnce "twitter" "turses"
            withTerminalWithName spawnOnce "mail" "offlineimap"
            withTerminalWithName spawnOnce "mail" "mutt"
            browser
            onceInTerminalWithNameSleep 5 "htop" "htop -u david"
    -- }}}


-- vim: foldmethod=marker:
