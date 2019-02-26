module Programs where

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
import Terminal


-- {{{ general definitions and helper functions
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
  , "-fn", "Inconsolata-14:normal"]


dmenuArgsWithFuzzy :: [String]
dmenuArgsWithFuzzy = "-z" : dmenuArgs


applicationsPath :: String
-- applicationsPath = "/usr/share/applications/"
applicationsPath = "/nix/var/nix/profiles/system/sw/share/applications"


home :: FilePath -> X FilePath
home path = io $ fmap (++ "/" ++ path) getHomeDirectory


projectPath :: X String
projectPath = io $ fmap (++ "/Projekte") getHomeDirectory
-- }}}


-- {{{ quick access
dmenu :: X ()
dmenu = do
  selection <- D.menuArgs "dmenu" dmenuArgs Apps.names
  sequence_ $ Apps.programs selection

dmenuAll :: X ()
dmenuAll = io programNames >>= D.menuArgs "dmenu" dmenuArgs >>= spawnHere
  where
    programNames :: IO [String]
    programNames = fmap (sort . lines) $ args >>= flip (runProcessWithInput "stest") []
        where
        args :: IO [String]
        args = fmap ("-flx" :) path
            where
            path :: IO [String]
            path = fmap (splitOn ":") $ getEnv "PATH"


-- TODO repair fuzzy matching (if enabled, nothing gets ever selected)
-- dmenuProjectOrg = projectNames >>= D.menuArgs "dmenu" dmenuArgsWithFuzzy >>= openInEditor
dmenuProjectOrg = projectNames >>= D.menuArgs "dmenu" dmenuArgs >>= openInEditor
  where
    projectNames :: X [String]
    projectNames = lines <$> lsProjectPath
    lsProjectPath :: X String
    lsProjectPath = do
        p <- projectPath
        runProcessWithInput "ls" [p] []
    toOrgFile :: String -> X String
    toOrgFile name = fmap (++ "/\"" ++ name ++ "/" ++ name ++ ".org\"") projectPath
    openInEditor :: String -> X ()
    openInEditor "" = return ()
    openInEditor name = editorWith =<< toOrgFile name


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


-- my own scratchpad action (I like toggling workspace more than bringing
-- window): Toggle terminal workspace and start terminal if not yet existing.
toggleScratchpad :: X ()
toggleScratchpad = do
    stackSet <- fmap windowset get
    let currentWSTag = W.tag . W.workspace $ W.current stackSet
    if currentWSTag == "terminal"
    then toggleWS
    -- else (windows $ W.greedyView "terminal") >> (startIfNecessary)
    else (windows $ W.view "terminal") >> (startIfNecessary)

        where
        startIfNecessary :: X ()
        startIfNecessary = do
            stackSet <- fmap windowset get
            let numberOfWindows = length $ W.index stackSet
            if numberOfWindows == 0
            then runTerminalWithName "terminal"
            else return ()


toggleEditor :: X ()
toggleEditor = do
    stackSet <- fmap windowset get
    let currentWSTag = W.tag . W.workspace $ W.current stackSet
    if currentWSTag == "editor"
    then toggleWS
    -- else (windows $ W.greedyView "editor") >> (startIfNecessary)
    else (windows $ W.view "editor") >> (startIfNecessary)

        where
        startIfNecessary :: X ()
        startIfNecessary = do
            stackSet <- fmap windowset get
            let numberOfWindows = length $ W.index stackSet
            if numberOfWindows == 0
            then editor
            else return ()


toggleBrowser :: X ()
toggleBrowser = do
    stackSet <- fmap windowset get
    let currentWSTag = W.tag . W.workspace $ W.current stackSet
    if currentWSTag == "browser"
    then toggleWS
    -- else (windows $ W.greedyView "browser") >> (startIfNecessary)
    else (windows $ W.view "browser") >> (startIfNecessary)

        where
        startIfNecessary :: X ()
        startIfNecessary = do
            stackSet <- fmap windowset get
            let numberOfWindows = length $ W.index stackSet
            if numberOfWindows == 0
            then browser
            else return ()
-- }}}


-- {{{ xmonad utility functions
-- toggle a workspace (if not there, go there; if there, go to the last one)
-- close all windows on all workspaces
closeAll :: X ()
closeAll = do
    stackset <- fmap windowset get
    let allWindows = W.allWindows stackset
    mapM_ killWindow allWindows


-- toggle between two workspaces
-- toggle :: String -> X ()
-- toggle wsName = do
--     stackSet <- fmap windowset get
--     let currentWSTag = W.tag . W.workspace $ W.current stackSet
--     if currentWSTag == wsName
--     then toggleWS
--     else windows $ W.greedyView wsName
-- }}}


-- {{{ power management
-- poweroff the computer, close windows gracefully before
shutdown :: X ()
shutdown = do
    outReset
    closeAll
    shutdownSound
    spawn "sleep 9 && systemctl poweroff"


-- reboot the computer, close windows gracefully before
reboot :: X ()
reboot = do
    outReset
    closeAll
    shutdownSound
    spawn "sleep 9 && systemctl reboot"


-- lock screen and suspend the computer
suspend :: X ()
suspend = do
    lockScreen
    spawn "sleep 3 && systemctl suspend"


-- TODO remove '~'
shutdownSound :: X ()
shutdownSound = spawn "mplayer ~/.sound/borealis/Exit1_1.wav"
-- }}}


-- {{{ applications
editor :: X ()
editor = spawn "emacsclient -c -a emacs"


editorWith :: String -> X ()
editorWith file = spawn $ "emacsclient -a emacs " ++ file


browser :: X ()
browser = spawn "chromium"


documentViewer :: X ()
documentViewer = spawn "zathura"


musicPlayer :: X ()
musicPlayer = spawn "spotify"


fileManager :: X ()
fileManager = spawn "thunar"


ircClient :: X ()
ircClient = inTerminalWithName "ircClient" "weechat"


spotifyCtl :: String -> X ()
spotifyCtl cmd = (fmap (++ " " ++ cmd) $ home "Bin/spotifyctl") >>= spawn


youtubeViewer :: X ()
youtubeViewer = inTerminalWithName "youtube-viewer" "youtube-viewer"


jiu :: X ()
jiu = home "Documents/jiu/jiu.org" >>= editorWith


gtd :: X ()
gtd = home "TODO.org" >>= editorWith


gtdIn :: X ()
gtdIn = home "In.org" >>= editorWith
-- }}}


-- {{{ utilities
ejectTray :: X ()
ejectTray = spawn "eject"


insertTray :: X ()
insertTray = spawn "eject -t"


lockScreen :: X ()
lockScreen = spawn "slimlock"


passmenu :: X ()
passmenu = spawn $ "passmenu " ++ "'" ++ intercalate "' '" dmenuArgs ++ "'"


powerTop :: X ()
powerTop = inTerminalWithName "powertop" "sudo powertop"


putAwayMouse :: X ()
putAwayMouse = spawn "xdotool mousemove 0 1280"


scrotWin :: X ()
scrotWin = spawn "sleep 0.2; scrot -s -e \'gimp $f\'"


scrotFull :: X ()
scrotFull = spawn "scrot"


showNeo :: X ()
showNeo = spawn "feh ~/.neo.png"


xKill :: X ()
xKill = spawn "xkill"
-- }}}


-- {{{ sound
inToggle :: X ()
inToggle = spawn "amixer sset 'Capture' toggle"


outUp :: X ()
outUp = home "Bin/volume +3%" >>= spawn


outDown :: X ()
outDown = home "Bin/volume -3%" >>= spawn


outToggle :: X ()
outToggle = home "Bin/volume %" >>= spawn


outReset :: X ()
outReset = home "Bin/volume 30%" >>= spawn


pavuControl :: X ()
pavuControl = spawnHere "pavucontrol"


equalizer :: X ()
equalizer = spawnHere "pulseaudio-equalizer-gtk"
-- }}}


-- {{{ screen brightness
lightUp :: X ()
lightUp = spawn "light -A 10"


lightDown :: X ()
lightDown = spawn "light -U 10"
-- }}}


-- vim: foldmethod=marker:
