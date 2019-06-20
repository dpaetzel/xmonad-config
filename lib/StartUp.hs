module StartUp where


import XMonad
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.SpawnOnce (spawnOnce)


import Programs


startupHook' :: String -> X ()
startupHook' _ = do
  -- From the documentation for 'setWMName': “May be useful for making Java GUI
  -- programs work, just set WM name to LG3D.”
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr
