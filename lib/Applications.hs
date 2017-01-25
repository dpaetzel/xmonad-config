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
chromiumAppCommand = ("chromium --profile-directory='Profile 4' --app-id=" ++)


applications :: [Application]
applications =
  [ Application "Anki" "anki"
  , Application "ARandR" "arandr"
  , Application "Calendar" $ chromiumAppCommand calendarAppID
  , Application "Chromium" "chromium"
  , Application "Emacs" "emacsclient -c -a emacs"
  , Application "Gimp" "gimp"
  , Application "MediathekView" "mediathekview"
  , Application "LibreOffice" "libreoffice"
  , Application "Signal" $ chromiumAppCommand signalAppID
  , Application "Telegram" "telegram-desktop"
  , Application "VLC" "vlc"
  ]

names :: [String]
names = map name $ applications

command :: String -> Maybe String
command name' = case filter (\app -> name app == name') applications of
  app : _ -> Just $ cmd app
  [] -> Nothing
