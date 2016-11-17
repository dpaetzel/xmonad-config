module Applications where

data Application = Application { name :: String
                               , cmd :: String
                               }


-- we'll see how stable chrome app-ids are…
signalAppID :: String
signalAppID = "bikioccmkafdpakkkcpdbppfkghcmihk"


telegramAppID :: String
telegramAppID = "clhhggbfdinjmjhajaheehoeibfljjno"


chromiumAppCommand :: String -> String
chromiumAppCommand = ("chromium --profile-directory='Profile 4' --app-id=" ++)


applications :: [Application]
applications =
  [ Application "Anki" "anki"
  , Application "Chromium" "chromium"
  , Application "Emacs" "emacsclient -c -a emacs"
  , Application "Gimp" "gimp"
  , Application "LibreOffice" "libreoffice"
  , Application "Signal" $ chromiumAppCommand signalAppID
  , Application "Telegram" $ chromiumAppCommand telegramAppID
  , Application "VLC" "vlc"
  ]

names :: [String]
names = map name $ applications

command :: String -> Maybe String
command name' = case filter (\app -> name app == name') $ applications of
  app : _ -> Just $ cmd app
  [] -> Nothing
