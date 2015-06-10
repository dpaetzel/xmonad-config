module Programs where


import Data.List
import Data.List.Split
import Data.Time
import System.Directory
import System.Environment
import Text.Regex.Posix
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Dmenu as D


-- TODO add proper user directory usage (do not use "~", rather use getHomeDirectory


dmenuArgs :: [String]
dmenuArgs = ["-l", "16", "-i", "-nb", "#000000", "-nf", "#729fcf", "-sb", "#000000", "-sf", "#ffffff", "-fn", "Inconsolata-14:normal"]
-- #729fcf is tango blue


applicationsPath :: String
applicationsPath = "/usr/share/applications/"


-- dmenu_run-alike but with spawnHere
dmenu :: X ()
-- dmenu = programNames >>= D.menuArgs "dmenu" dmenuArgs >>= io executable >>= spawnHere
dmenu = (io programNames) >>= D.menuArgs "dmenu" dmenuArgs >>= (io . executable) >>= spawnHere
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


addInNote :: X ()
addInNote = do
    io appendDate
    io filename >>= appendFilePrompt myXPConfig
      where
      appendDate :: IO ()
      appendDate = do
          f <- filename
          d <- currentDate
          appendFile f (d ++ " ")
      currentDate :: IO String
      currentDate = fmap (show . utctDay) getCurrentTime
      filename :: IO String
      filename = fmap (++ "/in") getHomeDirectory
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
            then spawnHere terminal''
            else return ()


-- toggle a workspace (if not there, go there; if there, go to the last one)
toggle :: String -> X()
toggle wsName = do
    stackSet <- fmap windowset get
    let currentWSTag = W.tag . W.workspace $ W.current stackSet
    if currentWSTag == wsName
    then toggleWS
    else windows $ W.greedyView wsName


-- close all windows on all workspaces
closeAll :: X ()
closeAll = do
    stackset <- fmap windowset get
    let allWindows = W.allWindows stackset
    mapM_ killWindow allWindows


-- poweroff the computer, close windows gracefully before
shutdown :: X ()
shutdown = do
    spawn outReset
    closeAll
    spawn shutdownSound
    spawn "sleep 9 && systemctl poweroff"


-- reboot the computer, close windows gracefully before
reboot :: X ()
reboot = do
    spawn outReset
    closeAll
    spawn shutdownSound
    spawn "sleep 9 && systemctl reboot"


-- lock screen and suspend the computer
suspend :: X ()
suspend = do
    spawn lockScreen
    spawn "sleep 3 && systemctl suspend"


-- main programs
terminal'      = "urxvt -uc"
terminal''     = "urxvt -uc -name terminal -title terminal"
terminalWith windowName command  = unwords [terminal', "-name", windowName, "-title", windowName, "-e", command]
-- documentViewer = "evince"
documentViewer = "zathura"
browser        = "firefox"
editor         = "gvim"
-- musicPlayer    = terminalWith "musicPlayer" "cmus"
musicPlayer    = "spotify"
mailClient     = terminalWith "mailClient" "mutt"
fileManager    = "thunar"
ircClient      = terminalWith "ircClient" "weechat"
jiu            = unwords [editor, "~/Documents/jiu/jiu.org"]
gtd            = unwords [editor, "~/todo.org"]
gtdIn          = unwords [editor, "~/in"]


-- util
-- dmenu         = "source ~/.zshenv; dmenu_run -i -nb '#000000' -nf '#ffffff' -sb '#ffffff' -sf '#000000' -fn 'Roboto-14:normal'"
dropboxToggle = "if (pgrep dropbox); then dropbox stop; sleep 5; killall dropbox; else dropbox start; fi"
ejectTray     = "eject"
insertTray    = "eject -t"
lockScreen    = "xscreensaver-command -lock"
powerTop      = terminalWith "powertop" "sudo powertop"
putAwayMouse  = "xdotool mousemove 0 1280"
scrotWin      = "sleep 0.2; scrot -s -e \'gimp $f\'"
scrotFull     = "scrot -e \'mv $f ~/Pictures/screenshots/not-yet-archived/\'"
xKill         = "xkill"


-- sound
inToggle      = "amixer sset 'Capture' toggle"
outUp         = "~/Bin/sound/change_volume.sh +3%"
outDown       = "~/Bin/sound/change_volume.sh -3%"
outToggle     = "~/Bin/sound/change_volume.sh %"
outReset      = "~/Bin/sound/change_volume.sh 30%"
-- outUp         = "~/Share/git/hasu/Volume/volume +"
-- outDown       = "~/Share/git/hasu/Volume/volume -"
-- outToggle     = "~/Share/git/hasu/Volume/volume %"
pavuControl   = "pavucontrol"
equalizer     = "pulseaudio-equalizer-gtk"
applause      = "mplayer -endpos 3 ~/Music/effects/applause.mp3"
startupSound  = "mplayer ~/Share/confs/sound/borealis/Startup1_2.wav"
shutdownSound = "mplayer ~/Share/confs/sound/borealis/Exit1_1.wav"


-- media
mocNext       = "mocp -f"
mocPrev       = "mocp -r"
mocPlay       = "mocp -G"
spotifyCtl    = ("~/Bin/user_interface/spotifyctl.sh " ++)
-- next          = "if (pgrep mocp); then mocp -f; else ~/Bin/spotify_control next; fi"
-- prev          = "if (pgrep mocp); then mocp -r; else ~/Bin/spotify_control previous; fi"
-- pause         = "if (pgrep mocp); then mocp -f; else ~/Bin/spotify_control playpause; fi"
youtubeViewer = terminalWith "youtube-viewer" "youtube-viewer"
spotify        = "spotify"


-- other
toggleTrayer  = "~/Bin/user_interface/toggle_trayer.sh"


-- TODO: make list and map over it in startuphook!
-- autostart
conky             = "conky"
dropbox           = "dropbox start"
dunst             = "dunst -print >> ~/.dunst.log"
-- jabberClient      = "padsp gajim"
jabberClient      = "pidgin"
japaneseInput     = "fcitx"
htop              = terminalWith "htop" "htop -u $USER"
-- kbLayout          = "setxkbmap -layout \"us, de\" -option \"grp:caps_toggle\""
mousePointer      = "xsetroot -cursor_name left_ptr"
myBackground      = "~/Bin/bg-set"
nmApplet          = "nm-applet"
noBell            = "xset -b"
offlineimap       = terminalWith "offlineimap" "offlineimap"
pulseaudio        = "start-pulseaudio-x11"
singleColorbg     = "xsetroot -solid black"
telegramClient    = "telegram"
-- trayer            = toggleTrayer
unclutter         = "unclutter -idle 5 -root"
xcompmgr          = "xcompmgr"
xflux             = "xflux -l 48.3 -g 10.9 -k 4000"
xmodmap           = "xmodmap ~/.Xmodmap"
xscreensaver      = "xscreensaver -no-splash"
