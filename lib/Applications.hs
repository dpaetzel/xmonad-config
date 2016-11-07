module Applications where

data Application = Application { name :: String
                               , cmd :: String
                               }

-- applications :: String ->  [Application]
-- applications host = defaultApplications ++ case host of
--   "heraklit" ->
--     [ Application "Telegram" "chromium --profile-directory='Profile 4' --app-id=clhhggbfdinjmjhajaheehoeibfljjno"
--     ]

--   where defaultApplications =
--           [ Application "Chromium" "chromium"
--           ]
applications :: [Application]
applications =
  [ Application "Anki" "anki"
  , Application "Chromium" "chromium"
  , Application "Emacs" "emacsclient -c -a emacs"
  , Application "Gimp" "gimp"
  , Application "LibreOffice" "libreoffice"
  -- we'll see how stable chrome app-ids are…
  , Application "Signal" "chromium --profile-directory='Profile 4' --app-id=bikioccmkafdpakkkcpdbppfkghcmihk"
  , Application "Telegram" "chromium --profile-directory='Profile 4' --app-id=clhhggbfdinjmjhajaheehoeibfljjno"
  , Application "VLC" "vlc"
  ]

names :: [String]
names = map name $ applications

command :: String -> Maybe String
command name' = case filter (\app -> name app == name') $ applications of
  app : _ -> Just $ cmd app
  [] -> Nothing
