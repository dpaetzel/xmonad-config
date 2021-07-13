module Windows where

import XMonad
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.ManageDocks (manageDocks)
import XMonad.Hooks.ManageHelpers
  ( (-?>),
    composeOne,
    doCenterFloat,
    doFullFloat,
    isDialog,
    isFullscreen,
  )

manageHook' :: ManageHook
manageHook' =
  -- Required for `spawnOn`.
  manageSpawn
    -- Required for `ToggleStruts` and `avoidStruts`.
    <+> manageDocks
    -- Use `composeOne` instead of `composeAll` in order to only use the first
    -- match, that is the first returning a `Just` (and not merge all of the
    -- matching rules). https://bbs.archlinux.org/viewtopic.php?id=98695
    <+> composeOne
          [ isDialog -?> doFloat,
            -- TODO This should work but it does not.
            -- title =? "zoom_linux_float_video_window" -?> doCenterFloat,
            -- TODO Not sure whether I still want/need this
            -- isFullscreen -?> doFullFloat
            title =? "Firefox Preferences" -?> doFloat,
            title =? "Tab Mix Plus Options" -?> doFloat,
            title =? "FoxyProxy Standard" -?> doFloat,
            className =? "scalafx.application.AppHelper" -?> doFloat,
            className =? "sun-awt-X11-XDialogPeer" -?> doFloat,
            title =? "Volume Control" -?> doCenterFloat,
            className =? "Nm-openconnect-auth-dialog" -?> doFloat,
            title =? "Wireshark" -?> doFloat,
            className =? "Ibus-ui-gtk3" -?> doFloat,
            title =? "as_toolbar" -?> doFloat,
            stringProperty "WM_WINDOW_ROLE" =? "gimp-toolbox-color-dialog" -?> doCenterFloat,
            title =? "ObstacleTower" -?> doCenterFloat,
            title =? "2048" -?> doCenterFloat,
            title =? "Wayfinder" -?> doCenterFloat,
            title =? "Reachability" -?> doCenterFloat
          ]
-- TODO If above Zoom rule does not work due to the window changing its title,
-- then this should work but it does not.
-- dynWindowEvents =
--   dynamicTitle
--     ( composeOne
--         [ title =? "zoom_linux_float_video_window" -?> doCenterFloat
--         ]
--     )
--
-- TODO Adding this keybinding should work as well but it does not either
-- (lilyterm is always spawned).
-- ((winMask .|. shiftMask, xK_r),
--   raiseMaybe (spawn "lilyterm") (title =? "zoom_linux_float_video_window"))

