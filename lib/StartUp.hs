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
startupHook' host = do
    -- don't know why i do this
    setWMName "LG3D"

    -- screen configuration
    setDefaultCursor xC_left_ptr
    xfork $ screenSetup host
    spawn myBackground

    spawnOnce unclutter
    spawnOnce xcompmgr
    spawnOnce xflux
    spawnOnce xscreensaver

    -- other configuration
    spawnOnce pulseaudio
    spawnOnce noBell
    spawnOnce xmodmap
    spawnOnce dunst
    spawnOnceSleep 2 conky

    -- start clients
    spawnOnce terminal''
    spawnOnce offlineimap
    spawnOnce mailClient
    spawnOnce browser
    spawnOnce musicPlayer
    spawnOnceSleep 5 htop

    -- start host specific things
    spawnOnceSleepOn 7 dropbox "aristoteles"

    where
        spawnOnceSleep :: Double -> String -> X ()
        spawnOnceSleep t = spawnOnce . printf "sh -c 'sleep %f; exec %s'" (t :: Double)

        spawnOnceSleepOn :: Double -> String -> String -> X ()
        spawnOnceSleepOn t runOnHost = when (runOnHost == host) . spawnOnceSleep t
