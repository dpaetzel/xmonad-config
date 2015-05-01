module StartUp where


import Control.Concurrent
import Control.Monad (when)
-- import System.Posix.Unistd
import Text.Printf
import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Util.Cursor
-- import XMonad.Util.Run
import XMonad.Util.SpawnOnce

import Programs
import ScreenSetup


-- startup
startupHook' :: String -> X ()
startupHook' host = case host of


    "anaxagoras" -> do

        -- spawnOnce musicPlayer
        spawnOnceSleep 10 jabberClient
        spawnOnceSleep 10 telegramClient
        spawnOnceSleep 10 ircClient

        defaultStartupHook


    "heraklit" -> do

        -- screen configuration
        xfork $ screenSetup host
        spawnOnceSleep 10 telegramClient

        defaultStartupHook


    _ -> defaultStartupHook

    where
        spawnOnceSleep :: Double -> String -> X ()
        spawnOnceSleep t = spawnOnce . printf "sh -c 'sleep %f; exec %s'" (t :: Double)

        defaultStartupHook :: X ()
        defaultStartupHook = do

            -- don't know why i do this
            setWMName "LG3D"

            -- look and feel
            spawn myBackground
            setDefaultCursor xC_left_ptr
            spawnOnce dunst
            spawnOnce xcompmgr
            spawnOnce xscreensaver
            spawnOnce xmodmap
            spawnOnce japaneseInput
            spawnOnce unclutter
            spawnOnce xflux
            spawnOnce noBell
            spawnOnce nmApplet
            spawnOnceSleep 2 conky

            -- start applications
            spawnOnce offlineimap
            spawnOnce mailClient
            spawnOnce browser

            spawnOnceSleep 5 htop
