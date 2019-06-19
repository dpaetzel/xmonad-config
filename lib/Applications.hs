module Applications where


import XMonad
import XMonad.Actions.SpawnOn (spawnHere)


import Path
import Terminal


data Application = Application { name :: String
                               , progs :: [X ()]
                               }


applications :: [Application]
applications =
  [ Application "Anki"          [spawnHere "anki -b $HOME/.Anki"]
  , Application "ARandR"        [spawnHere "arandr"]
  , Application "Audacity"      [spawnHere "audacity"]
  , Application "Chrome"        [spawnHere "google-chrome-stable"]
  , Application "Chromium"      [spawnHere "chromium"]
  , Application "Emacs"         [editor]
  , Application "Firefox"       [spawnHere "firefox"]
  , Application "Gimp"          [spawnHere "gimp"]
  , Application "GVim"          [spawnHere "gvim"]
  , Application "MediathekView" [spawnHere "mediathekview"]
  , Application "NetLogo"       [spawnHere "netlogo"]
  , Application "LibreOffice"   [spawnHere "libreoffice"]
  , Application "Signal"        [spawnHere "signal-desktop"]
  , Application "Spacemacs"     [editor]
  , Application "Spotify"       [spawnHere "spotify"]
  , Application "Telegram"      [spawnHere "telegram-desktop"]
  , Application "Thunderbird"   [spawnHere "thunderbird"]
  , Application "Vim"           [spawnHere "gvim"]
  , Application "VirtualBox"    [spawnHere "VirtualBox"]
  , Application "VLC"           [spawnHere "vlc"]
  , Application "Windows"       [spawnHere "VirtualBoxVM --startvm 'Windows 10'"]
  , Application "Zathura"       [spawnHere "zathura"]

  , Application "E-Mail"        [inTerminalWithName "E-Mail" "neomutt"]
  , Application "Browser"       [spawnHere "firefox"]
  , Application "Editor"        [editor]
  , Application "Chat"
    [ spawnHere "signal-desktop"
    , spawnHere "telegram-desktop"
    , inTerminalWithName "E-Mail" "neomutt"
    ]
  ]
  where
    editor = (++ " -c") <$> home "Bin/v" >>= spawnHere


names :: [String]
names = map name applications


programs :: String -> [X ()]
programs name' =
  concatMap progs . filter (\app -> name app == name') $ applications
