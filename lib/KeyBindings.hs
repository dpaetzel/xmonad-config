module KeyBindings
  ( winMask,
    namedKeys,
    mouseBindings',
  )
where

import qualified Data.Map as M
import Graphics.X11.ExtraTypes
import Path
import Programs
import System.Exit (exitSuccess)
import Terminal
import Workspaces
import XMonad
import XMonad.Actions.CycleWS (swapNextScreen, toggleOrDoSkip, toggleWS)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Submap (subName, visualSubmap)
import XMonad.Actions.WindowBringer (gotoMenuArgs)
import XMonad.Actions.WindowGo (raise, raiseMaybe)
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ManageDocks
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Util.NamedActions as NA
import qualified XMonad.StackSet as W

winMask :: KeyMask
winMask = mod1Mask

appMask :: KeyMask
appMask = mod4Mask

myXPConfig = def {
    bgColor = "#000000",
    fgColor = "#ffffff",
    font = "xft: Inconsolata-14:normal",
    promptBorderWidth = 0
}


namedKeys host conf =
  -- main programs
  [ ((winMask, xK_b), addName "Terminal" runTerminal),
    ((appMask, xK_minus), addName "Open project in Emacs" dmenuProjectOrg),
    ((appMask, xK_e), addName "File manager" fileManager),
    ((appMask .|. shiftMask, xK_Return), addName "Open Inbox.md" openInbox),
    ((appMask, xK_Return), addName "Record note" $ addNote True),
    -- util
    ((appMask, xK_space), addName "Dmenu: Apps" dmenu),
    ((appMask .|. shiftMask, xK_space), addName "Dmenu: All" dmenuAll),
    ((appMask, xK_i), addName "Dmenu: Literatur" litmenu),
    -- TODO Use Submaps to solve All™ my keybinding problems
    ((winMask, xK_adiaeresis), addName "Screen" $ visualSubmap def . M.fromList $
      [ ((0, xK_l), subName "Go to left screen" $ viewScreen def (P 0)),
        ((0, xK_r), subName "Go to right screen" $ viewScreen def (P 1)),
        ((shiftMask, xK_l), subName "Send to left screen" $ sendToScreen def (P 0)),
        ((shiftMask, xK_r), subName "Send to right screen" $ sendToScreen def (P 1)),
        ((0, xK_s), subName "Swap screens" swapNextScreen)
      ]
    ),
    ((appMask, xK_l), addName "Lock screen" lockScreen)
  ]
  ^++^

    [
      -- util
      ((winMask, xK_a), putAwayMouse),
      ((appMask, xK_Print), home "5Code/utility/shot clip --disable-picom" >>= spawn),
      ((appMask .|. shiftMask, xK_Print), home "5Code/utility/shot open --disable-picom" >>= spawn),
      ((appMask, xK_n), showNeo),
      ((appMask, xK_b), dmenuBluetooth),
      ((appMask, xK_p), passmenu),
      ((appMask .|. shiftMask, xK_p), passmenuClip),
      ((appMask, xK_v), clipmenu),
      ((winMask, xK_ssharp), spawn "dunstctl close"),
      ((winMask .|. shiftMask, xK_ssharp), spawn "dunstctl history-pop"),
      -- sound
      ((0, xF86XK_AudioRaiseVolume), outUp),
      ((0, xF86XK_AudioLowerVolume), outDown),
      ((0, xF86XK_AudioMute), outToggle),
      ((0, xF86XK_AudioMicMute), inToggle),
      ((appMask, xK_m), pavuControl),
      -- screen brightness
      ((0, xF86XK_MonBrightnessUp), lightUp),
      ((0, xF86XK_MonBrightnessDown), lightDown),
      -- music
      ((0, xF86XK_AudioPlay), spotifyCtl "playpause"),
      ((0, xF86XK_AudioNext), spotifyCtl "next"),
      ((0, xF86XK_AudioPrev), spotifyCtl "previous"),
      -- xdotool type and keyboard
      ((appMask, xK_t), spawn "shrug"),
      -- m is the only key that is the same on both `de` and `de neo`.
      ((winMask .|. appMask, xK_m), home "5Code/utility/detoggle" >>= spawn),
      -- windows
      -- Close focused window
      ((winMask .|. shiftMask, xK_q), kill),
      -- Move focus to the next window
      ((winMask, xK_j), windows W.focusDown),
      -- Move focus to the previous window
      ((winMask, xK_k), windows W.focusUp),
      -- Swap the focused window with the next window
      ((winMask .|. shiftMask, xK_j), windows W.swapDown),
      -- Swap the focused window with the previous window
      ((winMask .|. shiftMask, xK_k), windows W.swapUp),
      -- Move focus to the master window
      ((winMask, xK_c), windows W.focusMaster),
      -- Swap the focused window and the master window
      ((winMask .|. shiftMask, xK_c), windows W.swapMaster),
      -- Push window back into tiling
      ((winMask, xK_i), withFocused $ windows . W.sink),
      -- window finder
      ((winMask, xK_g), gotoMenuArgs dmenuArgs),
      -- layouts
      -- Rotate through the available layout algorithms
      ((winMask, xK_p), sendMessage NextLayout),
      -- Reset the layouts on the current workspace to default
      ((winMask .|. shiftMask, xK_p), setLayout $ XMonad.layoutHook conf),
      -- Shrink the master area
      ((winMask, xK_h), sendMessage Shrink),
      -- Expand the master area
      ((winMask, xK_l), sendMessage Expand),
      -- Increment the number of windows in the master area
      ((winMask, xK_comma), sendMessage (IncMasterN 1)),
      -- Decrement the number of windows in the master area
      ((winMask, xK_period), sendMessage (IncMasterN (-1))),
      -- Toggle bars
      ((winMask, xK_d), sendMessage ToggleStruts),
      -- ((winMask .|. controlMask, xK_h), sendMessage $ pullGroup L),
      -- ((winMask .|. controlMask, xK_l), sendMessage $ pullGroup R),
      -- ((winMask .|. controlMask, xK_k), sendMessage $ pullGroup U),
      -- ((winMask .|. controlMask, xK_j), sendMessage $ pullGroup D),
      -- ((winMask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll)),
      -- ((winMask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge)),
      -- ((winMask .|. controlMask, xK_period), onGroup W.focusUp'),
      -- ((winMask .|. controlMask, xK_comma), onGroup W.focusDown'),
      -- screens
      -- xmonad
      -- Quit xmonad
      ((winMask .|. shiftMask, xK_F12), closeAll >> io exitSuccess),
      -- Restart xmonad
      ((winMask, xK_F12), spawn "xmonad --recompile; xmonad --restart"),
      -- power management
      -- Suspend computer
      ((winMask, xK_Delete), suspend),
      -- Shutdown computer
      ((appMask .|. winMask .|. shiftMask, xK_Delete), shutdown),
      -- Reboot computer
      ((appMask .|. winMask .|. shiftMask, xK_Insert), reboot),
      -- workspaces
      ((winMask, xK_w), selectWorkspace myXPConfig),
      ((winMask .|. shiftMask, xK_w), withWorkspace myXPConfig (windows . W.shift)),
      ((appMask .|. winMask .|. shiftMask, xK_w), removeWorkspace),
      -- toogle last workspace
      ((winMask, xK_o), toggleWS)
    ]
      ^++^
      -- mod-[1..9,x,z,s,n], Switch to workspace N
      -- mod-shift-[1..9,x,z,s,n], Move client to workspace N
      [ ((winMask .|. mod, key), toWorkspace fun)
        | (key, toWorkspace) <-
            -- m for Müll
            [ (xK_m, flip ($) "trash"),
              (xK_z, flip ($) "chat"),
              (xK_s, flip ($) "browser"),
              (xK_t, flip ($) "terminal"),
              (xK_n, flip ($) "editor")
            ]
              ++ zip
                   (xK_dead_circumflex : [xK_1 .. xK_9] ++ [xK_0])
                   (map (flip ($)) generalPurposeWS),
          (fun, mod) <-
            -- [ (toggleOrView, 0) -- too greedy
            [ (toggleOrDoSkip [] W.view, 0),
              (windows . W.shift, shiftMask)
            ]
      ]
      ^++^
      -- not nice but this way both shifting windows to and toggling the scratchpad works
      [((winMask, xK_t), toggleScratchpad)]

-- mouse bindings
mouseBindings' _ =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (winMask .|. appMask, button1),
        \w ->
          focus w >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      -- mod-button2, Raise the window to the top of the stack
      -- ((winMask, button2), \w -> focus w >> windows W.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (winMask .|. appMask, button3),
        \w ->
          focus w >> mouseResizeWindow w
            >> windows W.shiftMaster
      )
    ]
