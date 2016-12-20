import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.ManageHook
import XMonad.Util.EZConfig
import XMonad.Util.Run

import Data.List
import Data.Maybe
import Data.Monoid
import System.Exit
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Layout.SingleSpacing


main = do
    panelHandle <- spawnPipe "~/.xmonad/dzen/panel.sh"

    xmonad $ withUrgencyHook focusHook $ baseConfig {
        keys            = \conf -> mkKeymap conf (myKeyBindings conf),
        layoutHook      = myLayout,
        manageHook      = myManageHook <+> manageHook baseConfig,
        handleEventHook = myEventHook <+> handleEventHook baseConfig,
        logHook         = myLogHook panelHandle <+> logHook baseConfig,
        startupHook     = myStartupHook <+> startupHook baseConfig
    }


baseConfig = desktopConfig {
    terminal           = "urxvtc -e tmux",
    focusFollowsMouse  = False,
    clickJustFocuses   = False,
    borderWidth        = 1,
    modMask            = mod4Mask,
    workspaces         = ["im", "web", "todo"] ++ map show [4..9],
    normalBorderColor  = "#534636",
    focusedBorderColor = "#D0C8C6"
}


myKeyBindings conf =
    -- Launchers
    [ ("M-<Return>", spawnHere $ XMonad.terminal conf)
    , ("M-S-<Return>", spawnHere "xterm")
    , ("M-o", spawnHere "j4-dmenu-desktop --dmenu=\"$HOME/bin/mydmenu -q -f\" --term=urxvtc")
    , ("M-r", spawnHere "urxvtc -e ranger")
    , ("M-i", spawnHere "rofi-pass")

    -- Quit xmonad
    , ("M-S-q", io exitSuccess)
    -- Restart xmonad
    , ("M-q", spawn "xmonad --recompile; xmonad --restart")

    -- Focus
    , ("M-<Tab>", windows W.focusDown)
    , ("M-S-<Tab>", windows W.focusUp)
    , ("M-m", windows W.focusMaster)
    , ("M-h", sendMessage $ Go L)
    , ("M-j", sendMessage $ Go D)
    , ("M-k", sendMessage $ Go U)
    , ("M-l", sendMessage $ Go R)

    -- Close focused window
    , ("M-c", kill)

    -- Layout management
    --   Rotate through the available layout algorithms
    , ("M-z", sendMessage NextLayout)
    --   Resize viewed windows to the correct size
    , ("M-S-r", refresh)
    --   Swap the focused window and the master window
    , ("M-<Space>", windows W.swapMaster)
    --   Swap the focused window with the window in the specified direction
    , ("M-S-h", sendMessage $ Swap L)
    , ("M-S-j", sendMessage $ Swap D)
    , ("M-S-k", sendMessage $ Swap U)
    , ("M-S-l", sendMessage $ Swap R)
    --   Shrink and expand the master area
    , ("M-C-h", sendMessage Shrink)
    , ("M-C-l", sendMessage Expand)
    --   Push window back into tiling
    , ("M-S-t", withFocused $ windows . W.sink)
    --   Increment/decrement the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))
    --   Toggle struts
    , ("M-b", sendMessage ToggleStruts)

    -- Workspace management
    , ("M-d", prevWS)
    , ("M-f", nextWS)
    , ("M-`", toggleWS)
    , ("M-S-d", shiftToPrev)
    , ("M-S-f", shiftToNext)
    , ("M-S-<Space>", moveTo Next EmptyWS)
    ] ++

    -- M-[1..9], Switch to workspace N
    -- M-S-[1..9], ("Moveclient to workspace N
    [("M-" ++ mask ++ show wsid, windows $ action workspace)
        | (workspace, wsid) <- zip (XMonad.workspaces conf) [1..9]
        , (action, mask) <- [(W.greedyView, ""), (W.shift, "S-")]] ++

    -- M-{w,e}, Switch to physical/Xinerama screens 1 or 2
    -- M-S-{w,e}, ("Moveclient to screen 1 or 2
    [("M-" ++ mask ++ key, screenWorkspace screen >>= flip whenJust (windows . action))
        | (screen, key) <- zip [0..] ["w", "e"]
        , (action, mask) <- [(W.view, ""), (W.shift, "S-")]] ++

    [ ("<XF86AudioRaiseVolume>", spawn "~/.xmonad/dzen/dvolume.sh -i 3")
    , ("<XF86AudioLowerVolume>", spawn "~/.xmonad/dzen/dvolume.sh -d 3")
    , ("<XF86AudioMute>", spawn "~/.xmonad/dzen/dvolume.sh -t")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86MonBrightnessUp>", spawn "~/.xmonad/dzen/dbrightness.sh +15")
    , ("<XF86MonBrightnessDown>", spawn "~/.xmonad/dzen/dbrightness.sh -15")
    ]


myLayout = modifiers layouts
  where
    modifiers = avoidStruts . smartBorders . spacing 15
                . desktopLayoutModifiers . windowNavigation
    layouts = Tall nmaster delta ratio ||| Full
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100


myManageHook :: ManageHook
myManageHook = composeAll
    [ manageSpawn
    , isDialog                      --> doFloat
    , isFullscreen                  --> doFullFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Xmessage"       --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ]


myEventHook :: Event -> X All
myEventHook = fullscreenEventHook <+> docksEventHook


myLogHook :: Handle -> X ()
myLogHook panelHandle = fadeInactiveLogHook 0.9 <+> updatePanel panelHandle

updatePanel :: Handle -> X ()
updatePanel panelHandle = dynamicLogWithPP $ def
    { ppCurrent         = \ws -> clickify ws $ pad $ dzenColor "#8AB3B5" "" ws
    , ppHidden          = \ws -> clickify ws $ pad $ dzenColor "" "#534636" ws
    , ppHiddenNoWindows = \ws -> clickify ws $ pad $ dzenColor "#7E705A" "" ws
    , ppLayout          = pad . dzenColor "#B8AFAD" ""
    , ppUrgent          = \ws -> clickify ws $ pad $ dzenColor "#F4BC87" "" ws
    , ppTitle           = dzenColor "#B8AFAD" "" . shorten 100
    , ppWsSep           = ""
    , ppSep             = pad . pad $ dzenColor "#534636" "" "^r(1x27)"
    , ppOutput          = hPutStrLn panelHandle
    }
    where
      clickify ws = wrap ("^ca(1,wmctrl -s " ++ show wsid ++ ")") "^ca()"
        where wsid = fromJust $ elemIndex ws (XMonad.workspaces baseConfig)


myStartupHook :: X ()
myStartupHook = do
    checkKeymap baseConfig (myKeyBindings baseConfig)
    addFullscreenSupport
    -- Temporary hack awaiting the struts cache fix for docks (in v0.13)
    mconcat $ replicate 2 $ sendMessage ToggleStruts

addFullscreenSupport :: X ()
addFullscreenSupport = withDisplay $ \dpy -> do
    wm <- asks theRoot
    supportProp <- getAtom "_NET_SUPPORTED"
    atomType <- getAtom "ATOM"
    fullscreenSupport <- getAtom "_NET_WM_STATE_FULLSCREEN"
    io $ changeProperty32 dpy wm supportProp atomType propModeAppend
                          [fromIntegral fullscreenSupport]
