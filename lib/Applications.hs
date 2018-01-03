module Applications where

data Application = Application { name :: String
                               , cmd :: String
                               }


-- we'll see how stable chrome app-ids are…
calendarAppID :: String
calendarAppID = "ejjicmeblgpmajnghnpcppodonldlgfn"


signalAppID :: String
signalAppID = "bikioccmkafdpakkkcpdbppfkghcmihk"


telegramAppID :: String
telegramAppID = "clhhggbfdinjmjhajaheehoeibfljjno"


chromiumAppCommand :: String -> String
chromiumAppCommand = ("chromium --app-id=" ++)


applications :: [Application]
applications =
  [ Application "Anki" "anki -b $HOME/.Anki"
  , Application "ARandR" "arandr"
  , Application "Audacity" "audacity"
  , Application "Calendar" $ chromiumAppCommand calendarAppID
  , Application "Chromium" "chromium"
  , Application "Chrome" "google-chrome-stable"
  , Application "Emacs" "emacsclient -c -a emacs"
  , Application "Gimp" "gimp"
  , Application "GVim" "gvim"
  , Application "MediathekView" "mediathekview"
  , Application "LibreOffice" "libreoffice"
  , Application "Signal" $ chromiumAppCommand signalAppID
  , Application "Spacemacs" "emacsclient -c -a emacs"
  , Application "Spotify" "spotify"
  , Application "Telegram" "telegram-desktop"
  , Application "Thunderbird" "thunderbird"
  , Application "Vim" "gvim"
  , Application "VLC" "vlc"
  , Application "Windows" "VirtualBox --startvm 'Windows 10'"
  , Application "Zathura" "zathura"

  , Application "E-Mail" "thunderbird"
  , Application "Browser" "chromium"
  , Application "Editor" "gvim"
  ]

names :: [String]
names = map name $ applications

command :: String -> Maybe String
command name' = case filter (\app -> name app == name') applications of
  app : _ -> Just $ cmd app
  [] -> Nothing
