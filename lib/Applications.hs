module Applications where


data Application = Application { name :: String
                               , cmds :: [String]
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
  [ Application "Anki"          ["anki -b $HOME/.Anki"]
  , Application "ARandR"        ["arandr"]
  , Application "Audacity"      ["audacity"]
  , Application "Calendar"      [chromiumAppCommand calendarAppID]
  , Application "Chromium"      ["chromium"]
  , Application "Chrome"        ["google-chrome-stable"]
  , Application "Emacs"         ["emacsclient -c -a emacs"]
  , Application "Gimp"          ["gimp"]
  , Application "GVim"          ["gvim"]
  , Application "MediathekView" ["mediathekview"]
  , Application "LibreOffice"   ["libreoffice"]
  , Application "Signal"        ["signal-desktop"]
  , Application "Spacemacs"     ["emacsclient -c -a emacs"]
  , Application "Spotify"       ["spotify"]
  , Application "Telegram"      ["telegram-desktop"]
  , Application "Thunderbird"   ["thunderbird"]
  , Application "Vim"           ["gvim"]
  , Application "VirtualBox"    ["VirtualBox"]
  , Application "VLC"           ["vlc"]
  , Application "Windows"       ["VirtualBox --startvm 'Windows 10'"]
  , Application "Zathura"       ["zathura"]

  , Application "E-Mail"        ["thunderbird"]
  , Application "Browser"       ["chromium"]
  , Application "Editor"        ["gvim"]
  , Application "Chat"          ["signal-desktop", "telegram-desktop"]
  ]


names :: [String]
names = map name applications


commands :: String -> [String]
commands name' =
  concatMap cmds . filter (\app -> name app == name') $ applications
