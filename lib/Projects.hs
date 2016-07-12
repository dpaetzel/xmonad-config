module Projects where

import XMonad.Actions.DynamicProjects

import Programs


projects :: [Project]
projects =
  [ Project { projectName = "browser"
            , projectDirectory = "~/Downloads"
            , projectStartHook = Just $ browser
            }
  ]
