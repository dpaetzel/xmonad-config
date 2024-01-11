module Programs where


import Control.Monad (when)
import Data.List (intercalate, sort)
import Data.List.Split (splitOn)
import Data.Time (getCurrentTime, utctDay)
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import Text.Regex.Posix ((=~))
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.SpawnOn (spawnHere)
import XMonad.Prompt
import XMonad.Util.Run (runProcessWithInput)
import XMonad.Util.SpawnOnce (spawnOnce)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dmenu as D


import Applications as Apps
import Editor
import Path
import Terminal


-- * General definitions and helper functions


dmenuArgs :: [String]
dmenuArgs =
  -- number of lines
  [ "-l", "10"
  -- case insensitive
  , "-i"
  -- screen dimming opacity
  -- , "-dim", "0.5"
  , "-nb", "#000000"
  , "-nf", "#729fcf"
  , "-sb", "#000000"
  , "-sf", "#ffffff"
  , "-fn", "Inconsolata\\ for\\ Powerline-14:normal"]


dmenuArgsWithFuzzy :: [String]
dmenuArgsWithFuzzy = "-z" : dmenuArgs


-- * Quick access


dmenu :: X ()
dmenu = do
  selection <- D.menuArgs "dmenu" dmenuArgs Apps.names
  sequence_ $ Apps.programs selection

dmenuAll :: X ()
dmenuAll = io programNames >>= D.menuArgs "dmenu" dmenuArgs >>= spawnHere
  where
    programNames :: IO [String]
    programNames =
      fmap (sort . lines) $ args >>= flip (runProcessWithInput "stest") []
        where
          args :: IO [String]
          args = fmap ("-flx" :) path
            where
              path :: IO [String]
              path = splitOn ":" <$> getEnv "PATH"


dmenuProjectOrg :: X ()
dmenuProjectOrg = projectNames >>= D.menuArgs "dmenu" dmenuArgs >>= openInEditor
  where
    projectNames :: X [String]
    projectNames = lines <$> lsProjectPath
    lsProjectPath :: X String
    lsProjectPath = do
      p <- projectPath
      runProcessWithInput "ls" [p] []
    toOrgFile :: String -> X String
    toOrgFile name =
      fmap (++ "/\"" ++ name ++ "/" ++ name ++ ".org\"") projectPath
    openInEditor :: String -> X ()
    openInEditor "" = return ()
    openInEditor name = editorWith =<< toOrgFile name


dmenuBluetooth :: X ()
dmenuBluetooth =
  home ("Bin/btmenu " ++ "'" ++ intercalate "' '" dmenuArgs ++ "'") >>= spawn


data Note = Note
instance XPrompt Note where
  showXPrompt Note = "In.org < "


addNote :: Bool -> X ()
addNote withDate = mkXPrompt Note myXPConfig complFun appendToIn
  where
    complFun :: String -> IO [String]
    complFun = return . const []
    appendToIn :: String -> X ()
    appendToIn "" = return ()
    appendToIn note = io $ do
      date <- fmap (show . utctDay) getCurrentTime
      file <- fmap (++ "/In.org") getHomeDirectory
      if withDate
      then appendFile file ("* [" ++ date ++ "] " ++ note ++ "\n")
      else appendFile file ("* " ++ note ++ "\n")
    myXPConfig = def {
      bgColor = "#000000",
      fgColor = "#ffffff",
      font = "xft: Inconsolata-14:normal",
      promptBorderWidth = 0
    }


{-|
My own scratchpad action: Toggle my terminal workspace and start a terminal if
there is no window there.

I like toggling workspace more than bringing a window to me.
-}
toggleScratchpad :: X ()
toggleScratchpad = do
  stackSet <- fmap windowset get
  let currentWSTag = W.tag . W.workspace $ W.current stackSet
  if currentWSTag == "terminal"
  then toggleWS
  else windows (W.view "terminal") >> startIfNecessary
    where
      startIfNecessary :: X ()
      startIfNecessary = do
        stackSet <- fmap windowset get
        let numberOfWindows = length $ W.index stackSet
        when (numberOfWindows == 0) $
          runTerminalWithName "terminal"


-- * Utility functions for XMonad manipulation


{-|
close all windows on all workspaces.
-}
closeAll :: X ()
closeAll = do
  stackset <- fmap windowset get
  let allWindows = W.allWindows stackset
  mapM_ killWindow allWindows


-- {-|
-- Toggles a workspace (if not there, go there; if there, go to the previous
-- one).
-- -}
-- toggle :: String -> X ()
-- toggle wsName = do
--   stackSet <- fmap windowset get
--   let currentWSTag = W.tag . W.workspace $ W.current stackSet
--   if currentWSTag == wsName
--   then toggleWS
--   else windows $ W.greedyView wsName


-- * Power management


{-|
Closes all windows gracefully and powers off the system.
-}
shutdown :: X ()
shutdown = do
  outReset
  closeAll
  spawn "sleep 9 && systemctl poweroff"


{-|
Closes all windows gracefully and reboots the system.
-}
reboot :: X ()
reboot = do
    outReset
    closeAll
    spawn "sleep 9 && systemctl reboot"


{-|
Locks the screen and suspends the computer
-}
suspend :: X ()
suspend = do
    lockScreen
    spawn "sleep 3 && systemctl suspend"


-- * Applications


fileManager :: X ()
fileManager = spawn "thunar"


spotifyCtl :: String -> X ()
spotifyCtl cmd = ((++ " " ++ cmd) <$> home "Bin/spotifyctl") >>= spawn


gtd :: X ()
gtd = home "TODO.org" >>= editorWith


gtdIn :: X ()
gtdIn = home "In.org" >>= editorWith


-- * Utility programs


lockScreen :: X ()
lockScreen = spawn "slock"


passmenu :: X ()
passmenu =
  home ("Bin/passmenu --type " ++ "'" ++ intercalate "' '" dmenuArgs ++ "'") >>= spawn


passmenuClip :: X ()
passmenuClip =
  home ("Bin/passmenu " ++ "'" ++ intercalate "' '" dmenuArgs ++ "'") >>= spawn


putAwayMouse :: X ()
putAwayMouse = spawn "xdotool mousemove --polar 135 10000"


scrotName :: String
scrotName = "\"$(date +'%F %R Screenshot $wx$h.png')\""


scrotWin :: X ()
scrotWin = spawn $ "sleep 0.2; scrot --freeze --select --line mode=classic,style=dash,width=3,color=red --exec 'gimp \"$f\"' " ++ scrotName

scrotWinClip :: X ()
scrotWinClip = spawn $ "sleep 0.2; " ++
  "scrot --freeze --select --line mode=classic,style=dash,width=3,color=red - " ++
  "| xclip -selection clipboard -t image/png"


scrotFull :: X ()
scrotFull = spawn $ "scrot " ++ scrotName


showNeo :: X ()
showNeo = spawn "feh ~/.neo.png"


-- * Sound management


inToggle :: X ()
inToggle = spawn "amixer sset 'Capture' toggle"


outUp :: X ()
outUp = spawn "pactl set-sink-volume \"alsa_output.pci-0000_00_1f.3.analog-stereo\" +3%"


outDown :: X ()
outDown = spawn "pactl set-sink-volume \"alsa_output.pci-0000_00_1f.3.analog-stereo\" -3%"


outToggle :: X ()
outToggle = spawn "amixer -q set Master toggle"


outReset :: X ()
outReset = spawn "pactl set-sink-volume \"alsa_output.pci-0000_00_1f.3.analog-stereo\" 30%"


pavuControl :: X ()
pavuControl = spawnHere "pavucontrol"


-- * Screen brightness management


lightUp :: X ()
lightUp = spawn "light -A 10"


lightDown :: X ()
lightDown = spawn "light -U 10"
