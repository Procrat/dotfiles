{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

import           Data.List                      (elemIndex)
import           Data.Maybe                     (fromJust)
import           Data.Monoid                    (All)
import           System.Exit                    (exitSuccess)
import qualified System.IO                      as IO

import           XMonad
import           XMonad.Actions.CycleWS         (Direction1D (..), WSType (..),
                                                 moveTo, nextWS, prevWS,
                                                 shiftToNext, shiftToPrev,
                                                 toggleWS)
import           XMonad.Actions.SpawnOn         (manageSpawn, spawnHere)
import           XMonad.Config.Desktop          (desktopConfig,
                                                 desktopLayoutModifiers)
import qualified XMonad.Hooks.DynamicLog        as DL
import           XMonad.Hooks.EwmhDesktops      (fullscreenEventHook)
import           XMonad.Hooks.FadeInactive      (fadeInactiveLogHook)
import           XMonad.Hooks.ManageDocks       (docksEventHook)
import qualified XMonad.Hooks.ManageHelpers     as MH
import           XMonad.Hooks.UrgencyHook       (focusHook, withUrgencyHook)
import           XMonad.Layout.NoBorders        (smartBorders)
import           XMonad.Layout.WindowNavigation (Direction2D (..),
                                                 Navigate (..),
                                                 windowNavigation)
import qualified XMonad.StackSet                as W
import           XMonad.Util.Dmenu              (menu)
import qualified XMonad.Util.EZConfig           as EZ
import           XMonad.Util.Run                (spawnPipe)

import qualified XMonad.Actions.Contexts        as C
import           XMonad.Layout.SingleSpacing    (spacing)


main :: IO ()
main = do
    panelHandle <- spawnPipe "~/.xmonad/dzen/panel.sh"

    xmonad $ withUrgencyHook focusHook $ baseConfig {
        keys            = \conf -> EZ.mkKeymap conf (myKeyBindings conf),
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
    workspaces         = ["im", "todo"] ++ map show ([3..9] :: [Int]),
    normalBorderColor  = "#534636",
    focusedBorderColor = "#D0C8C6"
}


myKeyBindings :: forall (l :: * -> *). XConfig l -> [(String, X ())]
myKeyBindings conf =
    -- Launchers
    [ ("M-<Return>", spawnHere $ terminal conf)
    , ("M-S-<Return>", spawnHere "xterm")
    , ("M-o", spawnHere myProgramLauncher)
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

    -- Context management
    , ("M-s", C.listContextNames >>= safeMenu >>= C.createAndSwitchContext)
    , ("M-S-s", C.listContextNames >>= safeMenu >>= C.deleteContext >> return ())

    -- Workspace management
    , ("M-d", prevWS)
    , ("M-f", nextWS)
    , ("M-`", toggleWS)
    , ("M-S-d", shiftToPrev)
    , ("M-S-f", shiftToNext)
    , ("M-S-<Space>", moveTo Next EmptyWS)
    ] ++

    -- M-[1..9], Switch to workspace N
    -- M-S-[1..9], Move client to workspace N
    [("M-" ++ mask ++ show wsid, windows $ action workspace)
        | (workspace, wsid) <- zip (workspaces conf) ([1..9] :: [Int])
        , (action, mask) <- [(W.greedyView, ""), (W.shift, "S-")]] ++

    -- M-{w,e}, Switch to physical/Xinerama screens 1 or 2
    -- M-S-{w,e}, Move client to screen 1 or 2
    [("M-" ++ mask ++ key, screenWorkspace screen >>= flip whenJust (windows . action))
        | (screen, key) <- zip [0..] ["w", "e"]
        , (action, mask) <- [(W.view, ""), (W.shift, "S-")]] ++

    -- Media keys
    [ ("<XF86AudioRaiseVolume>", spawn "~/.xmonad/dzen/dvolume.sh -i 3")
    , ("<XF86AudioLowerVolume>", spawn "~/.xmonad/dzen/dvolume.sh -d 3")
    , ("<XF86AudioMute>", spawn "~/.xmonad/dzen/dvolume.sh -t")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86MonBrightnessUp>", spawn "~/.xmonad/dzen/dbrightness.sh +15")
    , ("<XF86MonBrightnessDown>", spawn "~/.xmonad/dzen/dbrightness.sh -15")
    ]

myProgramLauncher :: String
myProgramLauncher =
    "j4-dmenu-desktop --dmenu=\"$HOME/bin/mydmenu -q -f\" --term=urxvtc"


myLayout = modifiers layouts
  where
    modifiers =
        desktopLayoutModifiers . smartBorders . spacing 15 . windowNavigation
    layouts = Tall 1 (3 / 100) (54 / 100) ||| Full


myManageHook :: ManageHook
myManageHook = composeAll
    [ manageSpawn
    , MH.isDialog                   --> doFloat
    , MH.isFullscreen               --> MH.doFullFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Xmessage"       --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    ]


myEventHook :: Event -> X All
myEventHook = fullscreenEventHook <+> docksEventHook


myLogHook :: IO.Handle -> X ()
myLogHook panelHandle = fadeInactiveLogHook 0.9 <+> updatePanel panelHandle

updatePanel :: IO.Handle -> X ()
updatePanel panelHandle = DL.dynamicLogWithPP $ def
    { DL.ppCurrent         = \ws -> clickify ws $ DL.pad $ DL.dzenColor "#8AB3B5" "" ws
    , DL.ppHidden          = \ws -> clickify ws $ DL.pad $ DL.dzenColor "#B8AFAD" "" ws
    , DL.ppHiddenNoWindows = \ws -> clickify ws $ DL.pad $ DL.dzenColor "#7E705A" "" ws
    , DL.ppLayout          = DL.pad . DL.dzenColor "#B8AFAD" ""
    , DL.ppUrgent          = \ws -> clickify ws $ DL.pad $ DL.dzenColor "#F4BC87" "" ws
    , DL.ppTitle           = DL.dzenColor "#B8AFAD" "" . DL.shorten 100
    , DL.ppWsSep           = ""
    , DL.ppSep             = DL.pad . DL.pad $ DL.dzenColor "#534636" "" "^r(1x27)"
    , DL.ppOutput          = IO.hPutStrLn panelHandle
    }
  where
    clickify ws = DL.wrap ("^ca(1,wmctrl -s " ++ show wsid ++ ")") "^ca()"
        where wsid = fromJust $ elemIndex ws (workspaces baseConfig)


myStartupHook :: X ()
myStartupHook = do
    EZ.checkKeymap baseConfig (myKeyBindings baseConfig)
    addFullscreenSupport
    -- Temporary hack awaiting the struts cache fix for docks (in v0.13)
    sendMessage ToggleStruts

addFullscreenSupport :: X ()
addFullscreenSupport = withDisplay $ \dpy -> do
    wm <- asks theRoot
    supportProp <- getAtom "_NET_SUPPORTED"
    atomType <- getAtom "ATOM"
    fullscreenSupport <- getAtom "_NET_WM_STATE_FULLSCREEN"
    io $ changeProperty32 dpy wm supportProp atomType propModeAppend
                          [fromIntegral fullscreenSupport]


safeMenu :: [String] -> X String
safeMenu options = do
    uninstallSignalHandlers
    choice <- menu "$HOME/bin/mydmenu" options
    installSignalHandlers
    return choice
