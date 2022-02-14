module KeyBindings
  ( winMask,
    keys',
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
import XMonad.Actions.WindowBringer (gotoMenuArgs)
import XMonad.Actions.WindowGo (raise, raiseMaybe)
import XMonad.Hooks.ManageDocks
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W

winMask :: KeyMask
winMask = mod1Mask

appMask :: KeyMask
appMask = mod4Mask

keys' host conf =
  M.fromList
    $
    -- main programs
    [ ((winMask, xK_b), runTerminal),
      ((appMask, xK_minus), dmenuProjectOrg),
      ((appMask, xK_e), fileManager),
      ((appMask, xK_u), gtd),
      ((winMask .|. appMask, xK_Return), gtdIn),
      -- TODO Currently broken, X session ends if I do this
      -- , ((appMask, xK_Return                                       ), addNote True)
      -- , ((appMask .|. shiftMask, xK_Return                         ), addNote False)

      -- util
      ((appMask, xK_space), dmenu),
      ((appMask .|. shiftMask, xK_space), dmenuAll),
      ((appMask, xK_l), lockScreen),
      ((winMask, xK_a), putAwayMouse),
      ((appMask, xK_Print), scrotWin),
      ((appMask .|. shiftMask, xK_Print), scrotFull),
      ((appMask, xK_n), showNeo),
      ((appMask, xK_b), dmenuBluetooth),
      ((appMask, xK_p), passmenu),
      ((appMask .|. shiftMask, xK_p), passmenuClip),
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
      -- xdotool type
      ((appMask, xK_t), home "Bin/shrug" >>= spawn),
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
      ((winMask, xK_w), windows W.focusMaster),
      -- Swap the focused window and the master window
      ((winMask .|. shiftMask, xK_w), windows W.swapMaster),
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
      -- swap screens
      ((winMask, xK_r), swapNextScreen),
      -- xmonad
      -- Quit xmonad
      ((winMask .|. shiftMask, xK_F12), closeAll >> io exitSuccess),
      -- Restart xmonad
      ((winMask, xK_F12), spawn "xmonad --recompile; xmonad --restart"),
      -- power management
      -- Suspend computer
      ((winMask, xK_Delete), suspend),
      -- Shutdown computer
      ((winMask .|. shiftMask, xK_Delete), shutdown),
      -- Reboot computer
      ((winMask .|. shiftMask, xK_Insert), reboot),
      -- workspaces
      -- toogle last workspace
      ((winMask, xK_o), toggleWS)
    ]
      ++
      -- mod-[1..9,x,z,s,n], Switch to workspace N
      -- mod-shift-[1..9,x,z,s,n], Move client to workspace N
      [ ((winMask .|. mod, key), toWorkspace fun)
        | (key, toWorkspace) <-
            [ (xK_x, flip ($) "trash"),
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
      ++
      -- not nice but this way both shifting windows to and toggling the scratchpad works
      [((winMask, xK_t), toggleScratchpad)]
      ++
      -- mod-{c,e,ä}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{c,e,ä}, Move client to screen 1, 2, or 3
      [ ((m .|. winMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_c, xK_e, xK_adiaeresis] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

-- mouse bindings
mouseBindings' _ =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (winMask, button1),
        \w ->
          focus w >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((winMask, button2), \w -> focus w >> windows W.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (winMask, button3),
        \w ->
          focus w >> mouseResizeWindow w
            >> windows W.shiftMaster
      )
    ]
