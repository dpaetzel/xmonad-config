module Terminal where


import Text.Printf (printf)
import XMonad
import XMonad.Util.SpawnOnce (spawnOnce)


terminalName :: String
terminalName = "lilyterm"


runTerminal :: X ()
runTerminal = spawn terminalName


runTerminalWithName :: String -> X ()
runTerminalWithName windowName =
    spawn $ unwords
        -- TODO generalize (see 14 lines below!)
        [ terminalName
        -- , "-name", windowName
        -- , "-title", windowName
        , "--title", windowName
        ]


inTerminalWithName :: String -> String -> X ()
inTerminalWithName = withTerminalWithName spawn


withTerminalWithName :: (String -> X ()) -> String -> String -> X ()
withTerminalWithName action windowName command =
    action $ unwords
        [ terminalName
        -- , "-name", windowName
        -- , "-title", windowName
        , "--title", windowName
        , "-e", command
        ]


onceInTerminalWithName :: String -> String -> X ()
onceInTerminalWithName = withTerminalWithName spawnOnce


onceInTerminalWithNameSleep :: Double -> String -> String -> X ()
onceInTerminalWithNameSleep t = withTerminalWithName (spawnOnceSleep t)


-- spawnOnce but sleep t seconds beforehand
spawnOnceSleep :: Double -> String -> X ()
spawnOnceSleep t = spawnOnce . printf "sh -c 'sleep %f; exec %s'" (t :: Double)
