module StartUp where

import XMonad
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.SpawnOnce (spawnOnce)

import Programs


-- startup
startupHook' :: String -> X ()
startupHook' host = case host of

    -- anaxagoras specific
    "anaxagoras" -> do

        spawn "/usr/local/bin/resetmsmice"
        spawnOnceSleep 10 "skype"
        spawnOnceSleep 10 "whatsie"

        defaultStartupHook

    -- heraklit specific
    "heraklit" ->

        -- spawnOnceSleep 3 "trayer\
        --                  \ --align center\
        --                  \ --margin 430\
        --                  \ --expand false\
        --                  \ --widthtype pixel\
        --                  \ --width 200\
        --                  \ --height 24\
        --                  \ --transparent true\
        --                  \ --alpha 0\
        --                  \ --tint 000000"
        -- spawnOnceSleep 5 "nm-applet"
        defaultStartupHook

    -- default
    _ -> defaultStartupHook

    where
        defaultStartupHook :: X ()
        defaultStartupHook = do

            -- don't know why i do this
            setWMName "LG3D"

            -- look and feel
            fmap ("feh --bg-scale " ++) (home "Pictures/wallpapers/current") >>= spawnOnce
            setDefaultCursor xC_left_ptr
            spawnOnce "parcellite"
            spawnOnce "xcompmgr"
            spawnOnce "unclutter -idle 5 -root"
            spawnOnce "redshift -l 48.3:10.9 -t 5500:2800"
            spawnOnce "xset -b"
            spawnOnceSleep 2 "conky"
            spawnOnceSleep 5 "dunst" -- not too early or the keybindings don't work

            -- start applications
            spawnOnceSleep 7 "emacsclient -c -a emacs"
            spawnOnceSleep 7 "chromium"
