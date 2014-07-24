module Programs where


import Data.List
import Data.List.Split
import Data.String.Utils
import System.Environment
import Text.Regex.Posix
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Util.Run
import qualified XMonad.Util.Dmenu as D


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
    programNames = fmap (map (replace ".desktop" "") . splitOn "\n") $ runProcessWithInput "ls" [applicationsPath] []
    executable :: String -> IO String
    executable programName = do
        (_, _, _, catch) <- match
        return $ head catch

        where
        match :: IO (String, String, String, [String])
        match = fmap (=~ regex) $ readFile (applicationsPath ++ programName ++ ".desktop")
        regex :: String
        regex = "\nExec=([^%]*)( .*\n|\n)"


dmenuAll :: X ()
dmenuAll = io programNames >>= D.menuArgs "dmenu" dmenuArgs >>= spawnHere
    where
    programNames :: IO [String]
    programNames = fmap (sort . splitOn "\n") $ args >>= flip (runProcessWithInput "stest") []
        where
        args :: IO [String]
        args = fmap ("-flx" :) path
            where
            path :: IO [String]
            path = fmap (splitOn ":") $ getEnv "PATH"


-- main programs
terminal'      = "urxvt -uc"
terminalWith windowName command  = intercalate " " [terminal', "-name", windowName, "-title", windowName, "-e", command]
documentViewer = "evince"
browser        = "firefox"
editor         = "gvim"
musicPlayer    = terminalWith "musicPlayer" "cmus"
mailClient     = terminalWith "mailClient" "mutt"
fileManager    = "thunar"
ircClient      = terminalWith "ircClient" "weechat"
todo           = intercalate " " [editor, "~/Todo/todo.org"]


-- util
-- dmenu         = "source ~/.zshenv; dmenu_run -i -nb '#000000' -nf '#ffffff' -sb '#ffffff' -sf '#000000' -fn 'Roboto-14:normal'"
dropboxToggle = "if (pgrep dropbox); then dropbox stop; sleep 5; killall dropbox; else dropbox start; fi"
ejectTray     = "eject"
insertTray    = "eject -t"
lockScreen    = "xscreensaver-command -lock"
powerTop      = terminalWith "powertop" "sudo powertop"
putAwayMouse  = "xdotool mousemove 1680 1280"
scrotWin      = "sleep 0.2; scrot -s -e \'mv $f ~/Pictures/screenshots/not-yet-archived/\'"
scrotFull     = "scrot -e \'mv $f ~/Pictures/screenshots/not-yet-archived/\'"
xKill         = "xkill"


-- sound
inToggle    = "amixer sset 'Capture' toggle"
outUp       = "~/Share/bin/sound/change_volume.sh +3%"
outDown     = "~/Share/bin/sound/change_volume.sh -3%"
outToggle   = "~/Share/bin/sound/change_volume.sh %"
-- outUp       = "~/Share/git/hasu/Volume/volume +"
-- outDown     = "~/Share/git/hasu/Volume/volume -"
-- outToggle   = "~/Share/git/hasu/Volume/volume %"
pavuControl = "pavucontrol"
equalizer   = "pulseaudio-equalizer-gtk"
applause    = "mplayer -endpos 3 ~/Music/effects/applause.mp3"


-- media
mocNext       = "mocp -f"
mocPrev       = "mocp -r"
mocPlay       = "mocp -G"
spotifyCtl    = ("~/Share/bin/user_interface/spotifyctl.sh " ++)
-- next          = "if (pgrep mocp); then mocp -f; else ~/Share/bin/spotify_control next; fi"
-- prev          = "if (pgrep mocp); then mocp -r; else ~/Share/bin/spotify_control previous; fi"
-- pause         = "if (pgrep mocp); then mocp -f; else ~/Share/bin/spotify_control playpause; fi"
youtubeViewer = terminalWith "youtube-viewer" "youtube-viewer"


-- other
toggleTrayer  = "~/Share/bin/user_interface/toggle_trayer.sh"


-- TODO: make list and map over it in startuphook!
-- autostart
conky             = "conky"
dropbox           = "dropbox start"
dunst             = "dunst -print >> ~/.dunst.log"
htop              = terminalWith "htop" "htop -u $USER"
-- kbLayout          = "setxkbmap -layout \"us, de\" -option \"grp:caps_toggle\""
mousePointer      = "xsetroot -cursor_name left_ptr"
myBackground      = "~/Share/bin/bg-set"
noBell            = "xset -b"
offlineimap       = terminalWith "offlineimap" "offlineimap"
pulseaudio        = "start-pulseaudio-x11"
singleColorbg     = "xsetroot -solid black"
-- trayer            = toggleTrayer
unclutter         = "unclutter -idle 5 -root"
xcompmgr          = "xcompmgr"
xflux             = "xflux -l 48.3714407 -g 10.8982552 -k 4000"
xmodmap           = "xmodmap ~/.xmodmap"
xscreensaver      = "xscreensaver -no-splash"
