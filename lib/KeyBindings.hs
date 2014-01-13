module KeyBindings
    (winMask
    , keys'
    , mouseBindings')
    where


import Control.Monad (void)
import qualified Data.Map as M
import Graphics.X11.ExtraTypes
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W


import Programs
import ScreenSetup


-- modkeys
winMask = mod1Mask
appMask = mod4Mask


-- key bindings
keys' host conf = M.fromList $

    -- main programs
    [ ((winMask, xK_Return                                       ), spawn terminal')
    , ((winMask .|. shiftMask, xK_Return                         ), spawn editor)
    , ((appMask, xK_o                                            ), spawn documentViewer)
    , ((appMask, xK_p                                            ), spawn musicPlayer)
    , ((appMask, xK_e                                            ), spawn fileManager)
    , ((appMask, xK_i                                            ), spawn ircClient)
    , ((appMask, xK_y                                            ), spawn youtubeViewer)


    -- util
    , ((appMask, xK_space                                        ), spawn dmenu)
    , ((appMask, xK_d                                            ), spawn dropboxToggle)
    , ((appMask, xK_Delete                                       ), spawn ejectTray)
    , ((appMask, xK_Insert                                       ), spawn insertTray)
    , ((appMask, xK_l                                            ), spawn lockScreen)
    , ((appMask, xK_b                                            ), spawn powerTop)
    , ((winMask, xK_a                                            ), spawn putAwayMouse)
    , ((appMask, xK_Print                                        ), spawn scrotWin)
    , ((0, xK_Print                                              ), spawn scrotFull)
    , ((appMask, xK_k                                            ), spawn xKill)


    -- sound
    , ((appMask, xK_Home                                         ), spawn inToggle)
    , ((appMask, xK_End                                          ), spawn outToggle)
    , ((0, xF86XK_AudioRaiseVolume                               ), spawn outUp)
    , ((0, xF86XK_AudioLowerVolume                               ), spawn outDown)
    , ((0, xF86XK_AudioMute                                      ), mapM_ spawn [outToggle, inToggle])
    , ((appMask, xK_m                                            ), spawn pavuControl)
    , ((appMask, xK_q                                            ), spawn equalizer)
    , ((appMask, xK_Page_Up                                      ), spawn applause)


    -- music
    -- , ((0, xF86XK_AudioNext                                   ), spawn mocNext)
    , ((0, xF86XK_AudioNext                                      ), spawn . spotifyCtl $ "next")
    -- , ((0, xF86XK_AudioPrev                                   ), spawn mocPrev)
    , ((0, xF86XK_AudioPrev                                      ), spawn . spotifyCtl $ "previous")
    -- , ((0, xF86XK_AudioPlay                                   ), spawn mocPlay)
    , ((0, xF86XK_AudioPlay                                      ), spawn . spotifyCtl $ "playpause")


    -- other
    -- , ((appMask, xK_t                                            ), spawn toggleTrayer)
    , ((appMask, xK_F4                                           ), io $ screenSetup host)
    , ((appMask, xK_r                                            ), sendMessage ToggleStruts)
    , ((winMask, xK_Tab                                          ), toggle "terminal")


    -- go to the next "xinerama" screen
    , ((winMask, xK_r                                            ), nextScreen)
    -- swap screens
    , ((winMask, xK_s                                            ), swapPrevScreen)
    -- toogle last workspace
    , ((winMask, xK_o                                            ), toggleWS)
    -- window finder
    , ((winMask, xK_semicolon                                    ), gotoMenu)
    -- close focused window
    , ((winMask .|. shiftMask, xK_q                              ), kill)
     -- Rotate through the available layout algorithms
    , ((winMask, xK_p                                            ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((winMask .|. shiftMask, xK_p                              ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((winMask, xK_n                                            ), refresh)
    -- Move focus to the next window
    , ((winMask, xK_j                                            ), windows W.focusDown)
    -- Move focus to the previous window
    , ((winMask, xK_k                                            ), windows W.focusUp  )
    -- Move focus to the master window
    , ((winMask, xK_m                                            ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((winMask .|. shiftMask, xK_m                              ), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((winMask .|. shiftMask, xK_j                              ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((winMask .|. shiftMask, xK_k                              ), windows W.swapUp    )
    -- Shrink the master area
    , ((winMask, xK_h                                            ), sendMessage Shrink)
    -- Expand the master area
    , ((winMask, xK_l                                            ), sendMessage Expand)
    -- Push window back into tiling
    , ((winMask, xK_t                                            ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((winMask, xK_comma                                        ), sendMessage (IncMasterN 1))
    -- Decrement the number of windows in the master area
    , ((winMask, xK_period                                       ), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((winMask .|. shiftMask, xK_F12                            ), io exitSuccess)
    -- Restart xmonad
    , ((winMask, xK_F12                                          ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((winMask .|. m, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) ([xK_grave] ++ [xK_1 .. xK_9] ++ [xK_0])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. winMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_w] [0..]
        --, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


-- mouse bindings
mouseBindings' _ = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((winMask, button1), \w -> focus w >> mouseMoveWindow w
        >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((winMask, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((winMask, button3), \w -> focus w >> mouseResizeWindow w
        >> windows W.shiftMaster)
    ]


-- toggle a workspace (if not there, go there; if there, go to the last one)
toggle :: String -> X()
toggle wsName = do
    stackSet <- fmap windowset get
    let currentWSTag = W.tag . W.workspace $ W.current stackSet
    if currentWSTag == wsName
    then toggleWS
    else windows $ W.greedyView wsName
