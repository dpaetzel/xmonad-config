module Programs where

-- main programs
terminal'      = "urxvt -uc"
terminalWith   = unwords . ([terminal'] ++) . return
terminal''     = terminalWith "-name terminal"
documentViewer = "evince"
browser        = "firefox"
editor         = "gvim"
musicPlayer    = terminalWith "-name musicPlayer -e 'cmus'"
mailClient     = terminalWith "-name mailClient -e mutt"
fileManager    = "thunar"
ircClient      = terminalWith "-name ircClient -e weechat"


-- util
dmenu         = "source ~/.zshenv; dmenu_run -i -nb '#000000' -nf '#ffffff' -sb '#ffffff' -sf '#000000' -fn 'Roboto-14:normal'"
ejectTray     = "eject"
insertTray    = "eject -t"
lockScreen    = "xscreensaver-command -lock"
powerTop      = terminalWith "-name powertop -e sudo powertop"
scrotWin      = "sleep 0.2; scrot -s -e \'mv $f ~/Pictures/screenshots/not-yet-archived/\'"
scrotFull     = "scrot -e \'mv $f ~/Pictures/screenshots/not-yet-archived/\'"
xKill         = "xkill"
dropboxToggle = "if (pgrep dropbox); then dropbox stop; sleep 5; killall dropbox; else dropbox start; fi"


-- sound
inToggle    = "amixer sset 'Capture' toggle"
-- outUp       = "amixer sset 'Master' 3%+"
-- outDown     = "amixer sset 'Master' 3%-"
-- outToggle   = "/home/david/Share/bin/user_interface/toggle_mute.rb"
outUp       = "~/Share/git/hasu/Volume/volume +"
outDown     = "~/Share/git/hasu/Volume/volume -"
outToggle   = "~/Share/git/hasu/Volume/volume %"
pavuControl = "pavucontrol"
equalizer   = "pulseaudio-equalizer-gtk"
applause    = "mplayer -endpos 3 ~/Music/effects/applause.mp3"


-- media
mocNext       = "mocp -f"
mocPrev       = "mocp -r"
mocPlay       = "mocp -G"
spotifyCtl    = ("/home/david/Share/bin/user_interface/spotify_control.sh " ++)
-- next          = "if (pgrep mocp); then mocp -f; else /home/david/Share/bin/spotify_control next; fi"
-- prev          = "if (pgrep mocp); then mocp -r; else /home/david/Share/bin/spotify_control previous; fi"
-- pause         = "if (pgrep mocp); then mocp -f; else /home/david/Share/bin/spotify_control playpause; fi"
youtubeViewer = terminalWith "-name youtube-viewer -e youtube-viewer"


-- other
toggleTrayer  = "/home/david/Share/bin/user_interface/toggle_trayer.sh"


-- TODO: make list and map over it in startuphook!
-- autostart
conky             = "conky"
dropbox           = "dropbox start"
dunst             = "dunst -print >> ~/.dunst.log"
htop              = terminalWith "-e htop -u david"
-- kbLayout          = "setxkbmap -layout \"us, de\" -option \"grp:caps_toggle\""
mousePointer      = "xsetroot -cursor_name left_ptr"
myBackground      = "/home/david/Share/bin/bg-set"
noBell            = "xset -b"
offlineimap       = terminalWith "-name offlineimap -e offlineimap"
pulseaudio        = "start-pulseaudio-x11"
singleColorbg     = "xsetroot -solid black"
-- trayer            = toggleTrayer
unclutter         = "unclutter -idle 5 -root"
xcompmgr          = "xcompmgr"
xflux             = "xflux -l 48.3714407 -g 10.8982552 -k 4000"
xmodmap           = "xmodmap ~/.xmodmap"
xscreensaver      = "xscreensaver -no-splash"
