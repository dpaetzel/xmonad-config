module Programs where


import Data.List
import Data.List.Split
import Data.Time
import System.Directory
import System.Environment
import Text.Printf
import Text.Regex.Posix
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Util.Paste (pasteSelection)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dmenu as D


-- {{{ general definitions and helper functions
terminalName :: String
terminalName = "urxvt -uc"


dmenuArgs :: [String]
dmenuArgs = ["-l", "16", "-i", "-nb", "#000000", "-nf", "#729fcf", "-sb", "#000000", "-sf", "#ffffff", "-fn", "Inconsolata-14:normal"]


applicationsPath :: String
applicationsPath = "/usr/share/applications/"


home :: FilePath -> X FilePath
home path = io $ fmap (++ "/" ++ path) getHomeDirectory


projectPath :: X String
projectPath = io $ fmap (++ "/Projects") getHomeDirectory
-- }}}


-- {{{ quick access
dmenu :: X ()
dmenu = io programNames >>= D.menuArgs "dmenu" dmenuArgs >>= io . executable >>= spawnHere
    where
    programNames :: IO [String]
    programNames = fmap (map removeSuffix . onlyDesktopFiles . lines) $ lsApplicationsPath
        where
        lsApplicationsPath :: IO String
        lsApplicationsPath = runProcessWithInput "ls" [applicationsPath] []
        onlyDesktopFiles :: [String] -> [String]
        onlyDesktopFiles = filter (=~ ".*\\.desktop")
        removeSuffix :: String -> String
        removeSuffix = reverse . drop 8 . reverse

    executable :: String -> IO String
    executable programName = do
        (_, _, _, catch) <- match
        return $ head catch
            where
            match :: IO (String, String, String, [String])
            match = fmap (command . execLine . lines) $ readFile (applicationsPath ++ programName ++ ".desktop")
            execLine :: [String] -> String
            execLine = head . filter (=~ "^Exec=.*$")
            command :: String -> (String, String, String, [String])
            command = (=~ "Exec=([^%]*)( |$)")


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


dmenuProjectOrg = projectNames >>= D.menuArgs "dmenu" dmenuArgs >>= openInEditor
    where
    projectNames :: X [String]
    projectNames = fmap (map deOrg . onlyOrg . lines) $ lsProjectPath
    lsProjectPath :: X String
    lsProjectPath = do
        p <- projectPath
        runProcessWithInput "ls" [p] []
    onlyOrg :: [String] -> [String]
    onlyOrg = filter (=~ ".*\\.org")
    deOrg :: String -> String
    deOrg = reverse . drop 4 . reverse
    toOrgFile :: String -> X String
    toOrgFile name = fmap (++ "/\"" ++ name ++ ".org\"") projectPath
    openInEditor :: String -> X ()
    openInEditor "" = return ()
    openInEditor name = editorWith =<< toOrgFile name


data Note = Note
instance XPrompt Note where
    showXPrompt Note = "In.org < "


clipmenuArgs = ["-l", "16", "-i", "-nb", "\\#000000", "-nf", "\\#729fcf", "-sb", "\\#000000", "-sf", "\\#ffffff", "-fn", "Inconsolata-14:normal"]
setClipboard :: X ()
setClipboard = spawn $ "clipmenu " ++ unwords clipmenuArgs
pasteClipboard :: X ()
pasteClipboard = pasteSelection


addNote :: X ()
addNote = mkXPrompt Note myXPConfig complFun appendToIn
    where
    complFun :: String -> IO [String]
    complFun = return . const []
    appendToIn :: String -> X ()
    appendToIn "" = return ()
    appendToIn note = io $ do
        date <- fmap (show . utctDay) getCurrentTime
        file <- fmap (++ "/In.org") getHomeDirectory
        appendFile file ("* " ++ note ++ "\n")
    myXPConfig = defaultXPConfig {
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
    else (windows $ W.greedyView "terminal") >> (startIfNecessary)

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
    else (windows $ W.greedyView "editor") >> (startIfNecessary)

        where
        startIfNecessary :: X ()
        startIfNecessary = do
            stackSet <- fmap windowset get
            let numberOfWindows = length $ W.index stackSet
            if numberOfWindows == 0
            then editor
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
toggle :: String -> X ()
toggle wsName = do
    stackSet <- fmap windowset get
    let currentWSTag = W.tag . W.workspace $ W.current stackSet
    if currentWSTag == wsName
    then toggleWS
    else windows $ W.greedyView wsName


-- spawnOnce but sleep t seconds beforehand
spawnOnceSleep :: Double -> String -> X ()
spawnOnceSleep t = spawnOnce . printf "sh -c 'sleep %f; exec %s'" (t :: Double)
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
shutdownSound = spawn "mplayer ~/Share/confs/sound/borealis/Exit1_1.wav"
-- }}}


-- {{{ terminal
runTerminal :: X ()
runTerminal = spawn terminalName


runTerminalWithName :: String -> X ()
runTerminalWithName windowName =
    spawn $ unwords
        -- TODO generalize (see 14 lines below!)
        [ terminalName
        , "-name", windowName
        , "-title", windowName
        ]


inTerminalWithName :: String -> String -> X ()
inTerminalWithName = withTerminalWithName spawn


withTerminalWithName :: (String -> X ()) -> String -> String -> X ()
withTerminalWithName action windowName command =
    action $ unwords
        [ terminalName
        , "-name", windowName
        , "-title", windowName
        , "-e", command
        ]


onceInTerminalWithName :: String -> String -> X ()
onceInTerminalWithName = withTerminalWithName spawnOnce


onceInTerminalWithNameSleep :: Double -> String -> String -> X ()
onceInTerminalWithNameSleep t = withTerminalWithName (spawnOnceSleep t)
-- }}}


-- {{{ applications
editorWith :: String -> X ()
editorWith file = spawn $ "emacsclient -a emacs " ++ file


documentViewer :: X ()
documentViewer = spawn "zathura"


editor :: X ()
editor = spawn "emacsclient -c -a emacs"


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
lockScreen = spawn "xscreensaver-command -lock"


powerTop :: X ()
powerTop = inTerminalWithName "powertop" "sudo powertop"


putAwayMouse :: X ()
putAwayMouse = spawn "xdotool mousemove 0 1280"


scrotWin :: X ()
scrotWin = spawn "sleep 0.2; scrot -s -e \'gimp $f\'"


-- TODO remove '~'
scrotFull :: X ()
scrotFull = spawn "scrot -e \'mv $f ~/Pictures/screenshots/not-yet-archived/\'"


xKill :: X ()
xKill = spawn "xkill"
-- }}}


-- {{{ sound
inToggle :: X ()
inToggle = spawn "amixer sset 'Capture' toggle"


outUp :: X ()
outUp = home "Bin/change-volume +3%" >>= spawn


outDown :: X ()
outDown = home "Bin/change-volume -3%" >>= spawn


outToggle :: X ()
outToggle = home "Bin/change-volume.sh %" >>= spawn


outReset :: X ()
outReset = home "Bin/change-volume 30%" >>= spawn


pavuControl :: X ()
pavuControl = spawnHere "pavucontrol"


equalizer :: X ()
equalizer = spawnHere "pulseaudio-equalizer-gtk"
-- }}}


-- vim: foldmethod=marker:
