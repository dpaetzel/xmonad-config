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

        spawn "/usr/local/bin/resetmsmice"
        spawnOnceSleep 10 "telegram"
        spawnOnceSleep 10 "skype"
        -- home "Bin/launchers/wa" >>= spawnOnceSleep 10
        spawnOnceSleep 10 "whatsie"

        defaultStartupHook
    -- }}}

    -- {{{ heraklit specific
    "heraklit" -> do

        -- inTerminalWithName "Telegram" "telegram-cli"
        spawnOnceSleep 10 "cutegram"

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
            spawnOnce "parcellite"
            spawnOnce "xcompmgr"
            spawnOnce "unclutter -idle 5 -root"
            spawnOnce "redshift -l 48.3:10.9 -t 5500:2800"
            spawnOnce "xset -b"
            spawnOnceSleep 2 "conky"
            spawnOnceSleep 5 "dunst" -- not too early or the keybindings don't work

            -- start applications
            withTerminalWithName spawnOnce "twitter" "turses"
            withTerminalWithName spawnOnce "mail" "offlineimap"
            withTerminalWithName spawnOnce "mail" "mutt"
            browser
            editor
            onceInTerminalWithNameSleep 5 "htop" "htop -u david"
    -- }}}


-- vim: foldmethod=marker:
