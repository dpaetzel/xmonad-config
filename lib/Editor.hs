module Editor where


import System.IO (FilePath)
import XMonad
import XMonad.Actions.SpawnOn (spawnHere)


import Path


{-|
In general, this is an X action because we might want to get a certain path
relative to home or something (and getting relative paths to home is an X
action).

Note that the prime versions of 'editorCommand', 'editor' and 'editorWith'
enforce creating a new editor window.
-}
editorCommand :: X FilePath
editorCommand = home "Bin/v"


{-|
Spawns the editor of choice.
-}
editor :: X ()
editor = editorCommand >>= spawnHere


{-|
Opens the given file in the editor.
-}
editorWith :: FilePath -> X ()
editorWith file = ((++ " " ++ file) <$> editorCommand) >>= spawn


{-|
Like 'editorCommand' but enforces creating a new editor window.
-}
editorCommand' :: X FilePath
editorCommand' = (++ " -c") <$> home "Bin/v"


{-|
Like 'editor' but enforces creating a new editor window.
-}
editor' :: X ()
editor' = editorCommand >>= spawnHere


{-|
Like 'editorWith' but enforces creating a new editor window.
-}
editorWith' :: String -> X ()
editorWith' file = ((++ " " ++ file) <$> editorCommand) >>= spawn
