{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures -fno-warn-unused-top-binds #-}

{-# LANGUAGE FlexibleInstances #-}

import           Control.Monad                  (mfilter, when)
import           Data.List                      (elemIndex)
import           Data.Monoid                    (All (..))
import           System.Exit                    (exitSuccess)
import           Text.Printf                    (printf)

import           XMonad                         hiding (title)
import qualified XMonad.Actions.CycleWS         as WS
import           XMonad.Actions.SpawnOn         (manageSpawn, spawnHere)
import           XMonad.Hooks.EwmhDesktops      (ewmh, ewmhFullscreen)
import           XMonad.Hooks.FadeInactive      (fadeInactiveLogHook)
import           XMonad.Hooks.ManageDocks       (avoidStruts, docks)
import qualified XMonad.Hooks.ManageHelpers     as MH
import qualified XMonad.Hooks.StatusBar         as SB
import qualified XMonad.Hooks.StatusBar.PP      as SB
import           XMonad.Hooks.UrgencyHook       (NoUrgencyHook (..),
                                                 withUrgencyHook)
import           XMonad.Layout.LayoutHints      (layoutHintsWithPlacement)
import           XMonad.Layout.NoBorders        (smartBorders)
import           XMonad.Layout.Renamed          (Rename (..), renamed)
import           XMonad.Layout.WindowNavigation (Direction2D (..),
                                                 Navigate (..),
                                                 windowNavigation)
import qualified XMonad.StackSet                as W
import           XMonad.Util.Cursor             (setDefaultCursor)
import           XMonad.Util.Dmenu              (menuArgs)
import qualified XMonad.Util.EZConfig           as EZ
import qualified XMonad.Util.NamedScratchpad    as NS
import           XMonad.Util.Run                (runProcessWithInput)

import qualified XMonad.Actions.Contexts        as C
import           XMonad.Layout.PseudoTiling     (doPseudoTile, pseudoTiling)
import qualified XMonad.Layout.PseudoTiling     as PseudoTiling
import           XMonad.Layout.SilenceModifier  (silenceLayoutModifier)
import           XMonad.Layout.SingleSpacing    (spacing)


main :: IO ()
main = do
    xmonad
    $ SB.withSB myStatusBar
    $ withUrgencyHook NoUrgencyHook
    $ ewmhFullscreen
    $ docks
    $ ewmh
    $ baseConfig {
        keys            = \conf -> EZ.mkKeymap conf (myKeyBindings conf),
        layoutHook      = myLayout,
        manageHook      = myManageHook <+> manageHook baseConfig,
        handleEventHook = myEventHook <+> handleEventHook baseConfig,
        logHook         = myLogHook <+> logHook baseConfig,
        startupHook     = startupHook baseConfig <+> myStartupHook
    }


baseConfig = def {
    terminal           = plainTerminal,
    focusFollowsMouse  = False,
    clickJustFocuses   = False,
    borderWidth        = 3,
    modMask            = mod4Mask,
    workspaces         = ["im", "todo", "music"] ++ map show ([4..9] :: [Int]),
    normalBorderColor  = "#3B3228",
    focusedBorderColor = "#7E705A"
}


myKeyBindings :: XConfig l -> [(String, X ())]
myKeyBindings conf =
    -- Launchers
    [ ("M-<Return>", mirrorTerminal >>= spawnHere)
    , ("M-S-<Return>", spawnHere plainTerminal)
    , ("M-C-<Return>", spawnHere "xterm")
    , ("M-o", spawnHere myProgramLauncher)
    , ("M-r", spawnHere "urxvtc -e ranger")
    , ("M-i", spawnHere "rofi-pass")
    , ("M-S-e", spawnHere "rofimoji --skin-tone neutral")
    , ("M-S-c", spawnHere "rofi -show calc -modi calc -no-show-match")
    , ("M-b", spawnHere "handlr launch x-scheme-handler/https")
    , ("M-p", NS.namedScratchpadAction myScratchpads "sound control")

    -- Quit xmonad
    , ("M-S-q", io exitSuccess)
    -- Recompile & restart xmonad
    , ("M-q", spawn "xmonad --restart")

    -- Focus
    , ("M-<Tab>", windows W.focusDown)
    , ("M-S-<Tab>", windows W.focusUp)
    , ("M-m", windows W.focusMaster)
    , ("M-h", sendMessage $ Go L)
    , ("M-j", sendMessage $ Go D)
    , ("M-k", sendMessage $ Go U)
    , ("M-l", sendMessage $ Go R)

    -- Close focused window and ensure Tall layout
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
    --   Push floating window back into tiling
    , ("M-S-t", withFocused $ windows . W.sink)
    --   Increment/decrement the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))
    , ("M-.", sendMessage (IncMasterN (-1)))

    -- Pseudotile
    , ("M-S-p", sendMessage PseudoTiling.ToggleFocusedWindow)

    -- Context management
    , ("M-s", C.listContextNames >>= safeMenu "Switch:" >>= C.createAndSwitchContext)
    , ("M-S-s", C.listContextNames >>= safeMenu "Remove:" >>= C.deleteContext >> return ())
    , ("M-S-v", C.showContextStorage)

    -- Workspace management
    , ("M-d", WS.prevWS)
    , ("M-f", WS.nextWS)
    , ("M-`", WS.toggleWS)
    , ("M-S-d", WS.shiftToPrev)
    , ("M-S-f", WS.shiftToNext)
    , ("M-S-<Space>", WS.moveTo WS.Next WS.emptyWS)
    ] ++

    -- M-[1..9], Switch to workspace N
    -- M-S-[1..9], Move client to workspace N
    [("M-" ++ mask ++ show wsid, windows $ action workspace)
        | (workspace, wsid) <- zip (workspaces conf) ([1..9] :: [Int])
        , (action, mask) <- [(W.greedyView, ""), (W.shift, "S-")]] ++

    -- M-{w,e}, Switch to physical/Xinerama screens 1 or 2
    [("M-" ++ key, screenWorkspace screen >>= flip whenJust (windows . W.view))
        | (screen, key) <- zip [0..] ["w", "e"]] ++

    -- Media keys
    [ ("<XF86AudioRaiseVolume>", spawn "~/.config/dzen/dvolume.sh -i 3")
    , ("<XF86AudioLowerVolume>", spawn "~/.config/dzen/dvolume.sh -d 3")
    , ("<XF86AudioMute>", spawn "~/.config/dzen/dvolume.sh -t")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86MonBrightnessUp>", spawn "~/.config/dzen/dbrightness.sh +15")
    , ("<XF86MonBrightnessDown>", spawn "~/.config/dzen/dbrightness.sh -15")

    -- Notification management
    , ("C-<Space>", spawn "dunstctl close")
    , ("C-S-<Space>", spawn "dunstctl close-all")
    , ("C-`", spawn "dunstctl history-pop")
    , ("C-<Return>", spawn "dunstctl action")
    ]

myProgramLauncher :: String
myProgramLauncher =
    "j4-dmenu-desktop --dmenu=\"$HOME/bin/mydmenu apps -q\" --term=urxvtc"


myLayout = modifiers layouts
  where
    modifiers =
        avoidStruts
        . smartBorders
        . silenceLayoutModifier (layoutHintsWithPlacement (0.5, 0.5))
        . pseudoTiling
        . spacing 15
        . windowNavigation
    layouts = normal ||| zoomed
    normal = renamed [Replace "Normal"] $ Tall 1 (3 / 100) (54 / 100)
    zoomed = renamed [Replace "Zoomed"] Full

instance Read (Layout Window) where
    readsPrec _ = readsLayout (Layout myLayout)


myManageHook :: ManageHook
myManageHook = composeAll
    [ manageSpawn
    , shouldFloat      --> doFloat
    , shouldIgnore     --> doIgnore
    , shouldPseudoTile --> doPseudoTile
    , NS.namedScratchpadManageHook myScratchpads
    ]
  where
    shouldFloat =
        MH.isDialog
        <||> appName =? "xmessage"
    shouldIgnore =
        appName =? "desktop_window"
        <||> appName =? "kdesktop"
    shouldPseudoTile = not <$>
        appName =? "emacs"
        <||> appName =? "brave-browser"
        <||> appName =? "chromium"
        <||> appName =? "Navigator"  -- Firefox
        <||> appName =? "rambox"
        <||> className =? "jetbrains-idea-ce"
        <||> appName =? "soffice"  -- LibreOffice


myScratchpads :: [NS.NamedScratchpad]
myScratchpads = [
    NS.NS "sound control" "pavucontrol" (className =? "Pavucontrol") MH.doCenterFloat
    ]


myEventHook :: Event -> X All
myEventHook = normalLayoutOnCloseEventHook

normalLayoutOnCloseEventHook :: Event -> X All
normalLayoutOnCloseEventHook DestroyWindowEvent{ev_window = window, ev_event = event} = do
    -- Not sure why, but DestroyWindowEvent is sometimes triggered when no
    -- window is being removed. It's always with the root window as the `event`
    -- though. Otherwise the event is set to the destroyed window. See
    -- https://tronche.com/gui/x/xlib/events/window-state-change/destroy.html.
    when (window == event) $ sendMessage (JumpToLayout "Normal")
    return $ All True
normalLayoutOnCloseEventHook _ = return $ All True


myLogHook :: X ()
myLogHook = fadeInactiveLogHook 0.9


myStatusBar :: SB.StatusBarConfig
myStatusBar = SB.statusBarProp "xmobar" (pure xmobarPP)

xmobarPP :: SB.PP
xmobarPP = withContextShown . SB.filterOutWsPP [NS.scratchpadWorkspaceTag] $ def
    { SB.ppCurrent         = styleWS "#8AB3B5"
    , SB.ppHidden          = styleWS "#B8AFAD"
    , SB.ppHiddenNoWindows = styleWS "#7E705A"
    , SB.ppUrgent          = styleWS "#F4BC87"
    , SB.ppSep             = SB.pad $ SB.xmobarColor "#534636" "" "<fn=1>│</fn>"

    , SB.ppWsSep           = ""
    , SB.ppTitle           = SB.shorten 100
    }
  where
    styleWS color ws = clickify ws $ SB.pad $ SB.xmobarColor color "" ws
    clickify ws =
        case elemIndex ws (workspaces baseConfig) of
          Nothing      -> id
          Just wsIndex -> SB.xmobarAction ("wmctrl -s " ++ show wsIndex) "1"

withContextShown :: SB.PP -> SB.PP
withContextShown pp = pp
    { SB.ppOrder  = putContextFirst . SB.ppOrder pp
    -- If we're on a non-default context, show its name
    , SB.ppExtras = SB.ppExtras pp ++ [fmap (fmap SB.pad) context]
    }
  where
    putContextFirst :: [String] -> [String]
    putContextFirst (ws:layout:title:extras) =
        if null extras
           then ws:layout:title:extras
           else last extras : ws:layout:title : init extras
    putContextFirst noExtras = noExtras
    context :: X (Maybe String)
    context = mfilter (/= C.defaultContextName)
        . Just <$> C.showCurrentContextName


myStartupHook :: X ()
myStartupHook = do
    EZ.checkKeymap baseConfig (myKeyBindings baseConfig)
    setEwmhDesktopGeometry
    setDefaultCursor xC_left_ptr

-- Set the desktop geometry.
-- Might not work for multiple monitors.
setEwmhDesktopGeometry :: X ()
setEwmhDesktopGeometry = withDisplay $ \dpy -> do
    wm <- asks theRoot

    atomType <- getAtom "ATOM"
    cardinalType <- getAtom "CARDINAL"

    supportProp <- getAtom "_NET_SUPPORTED"
    desktopGeometryProp <- getAtom "_NET_DESKTOP_GEOMETRY"

    io $ do
        changeProperty32 dpy wm supportProp atomType propModeAppend
                         [fromIntegral desktopGeometryProp]
        windowAttributes <- getWindowAttributes dpy wm
        let width = fromIntegral $ wa_width windowAttributes
            height = fromIntegral $ wa_height windowAttributes
        changeProperty32 dpy wm desktopGeometryProp cardinalType propModeReplace
                         [width, height]


safeMenu :: String -> [String] -> X String
safeMenu prompt options = do
    uninstallSignalHandlers
    choice <- menuArgs "/home/procrat/bin/mydmenu" ["default", "-p", prompt] options
    installSignalHandlers
    return choice


mirrorTerminal :: X String
mirrorTerminal = withWindowSet $ \ws ->
    case W.peek ws of
        Nothing -> return plainTerminal
        Just window -> do
            let hexWindowId = printf "0x%08x" window
            workingDir <- runProcessWithInput "/home/procrat/bin/winwd" [hexWindowId] ""
            return $ case workingDir of
                ""  -> plainTerminal
                dir -> terminalWithWorkingDir $ init dir

terminalWithWorkingDir :: String -> String
terminalWithWorkingDir dir = "urxvtc -e tmux new-session -c '" ++ dir ++ "'"

plainTerminal :: String
plainTerminal = "urxvtc -e tmux"
