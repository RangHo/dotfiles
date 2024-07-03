import System.Exit

import Control.Monad
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86

import XMonad hiding (mouseResizeWindow)

import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize
import XMonad.Actions.Navigation2D
import XMonad.Actions.TiledWindowDragging
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Gaps
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

-- ---------------------------------------------------------------------
-- Constants
altMask = mod1Mask
superMask = mod4Mask

-- ---------------------------------------------------------------------
-- Variables
myBorderWidth = 0
myFocusFollowsMouse = True
myFocusedBorderColor = "#eeeeee"
myGapSize = 20
myModMask = superMask
myNormalBorderColor = "#222222"
myScratchpads =
    [ NS "terminal" "wezterm start --class scratchpad" (className =? "scratchpad") focusedFloating
    ]
  where
    focusedFloating = customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
myTerminal = "wezterm"
myWorkspaces =
    [ "default"
    , "code"
    , "chat"
    , "media"
    , "misc1"
    , "misc2"
    , "misc3"
    , "misc4"
    , "misc5"
    ]

-- ---------------------------------------------------------------------
-- Applications
myBrowser = "firefox"
myFastEditor = "emacsclient -c"
mySlowEditor = "emacs"
myEmailClient = "thunderbird"
myFileExplorer = "Thunar"
myLauncher = "rofi -show drun"

-- ---------------------------------------------------------------------
-- Helper functions

-- | Check if a window is in floating state.
isFloating :: Window -> WindowSet -> Bool
isFloating w ws = M.member w (W.floating ws)

-- | Toggle the floating state of a window.
toggleFloat :: Window -> X ()
toggleFloat w = windows $ go w
  where
    go w ws =
        if isFloating w ws
            then W.sink w ws
            else W.float w floatRect ws
    floatRect = W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)

-- | Toggle fullscreen mode.
toggleFullscreen :: Window -> X ()
toggleFullscreen _ = do
    sendMessage $ Toggle "Full"
    sendMessage $ ToggleStruts

-- | Drag the window with the mouse, depending on the floating state.
myDragWindow :: Window -> X ()
myDragWindow w = do
    ws <- gets windowset
    if isFloating w ws
        then dragFloating w
        else dragTiled w
  where
    dragFloating w = do
        focus w
        mouseMoveWindow w
        windows W.shiftMaster
    dragTiled w = dragWindow w

-- | Resize the window with the mouse, depending on the floating state.
myResizeWindow :: Window -> X ()
myResizeWindow w = do
    ws <- gets windowset
    when (isFloating w ws) $ do
        focus w
        mouseResizeWindow w
        windows W.shiftMaster

-- ---------------------------------------------------------------------
-- Key bindings
myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf =
    M.fromList $
        -- XMonad management
        [ ((controlMask .|. altMask, xK_Delete), return ()) -- Ctrl + Alt + Delete to show power menu
        , ((controlMask .|. altMask, xK_BackSpace), io exitSuccess) -- Ctrl + Alt + Backspace to kill XMonad
        ]
            ++
            -- Letter shortcuts
            [ ((superMask, xK_q), kill) -- [q]uit the focused window
            , ((superMask, xK_w), spawn myBrowser) -- run [w]eb browser
            , ((superMask, xK_e), spawn myFileExplorer) -- run file [e]xplorer (windows compatible)
            , ((superMask, xK_r), spawn myLauncher) -- [r]un a program (windows compatible)
            , ((superMask, xK_t), spawn myFastEditor) -- run [t]ext editor
            , ((superMask .|. shiftMask, xK_t), spawn mySlowEditor) -- run [t]ext editor, but slower
            , ((superMask, xK_y), return ())
            , ((superMask, xK_u), return ())
            , ((superMask, xK_i), return ())
            , ((superMask, xK_o), return ())
            , ((superMask, xK_p), return ())
            , ((superMask, xK_a), return ())
            , ((superMask, xK_s), return ())
            , ((superMask, xK_d), return ())
            , ((superMask, xK_f), withFocused toggleFullscreen) -- toggle [f]ullscreen
            , ((superMask, xK_g), return ())
            , -- xK_h is reserved as an alternative to arrow keys
              -- xK_j is reserved as an alternative to arrow keys
              -- xK_k is reserved as an alternative to arrow keys
              -- xK_l is reserved as an alternative to arrow keys
              ((superMask, xK_z), return ())
            , ((superMask, xK_x), return ())
            , ((superMask, xK_c), return ())
            , ((superMask, xK_v), return ())
            , ((superMask, xK_b), return ())
            , ((superMask, xK_n), return ())
            , ((superMask, xK_m), spawn myEmailClient) -- run [m]ail client
            ]
            ++
            -- Number shortcuts
            -- super + 1-9 to switch to workspace 1-9
            -- super + shift + 1-9 to move window to workspace 1-9
            [ ((superMask .|. m, k), windows $ f w)
            | (k, w) <- zip [xK_1 .. xK_9] (XMonad.workspaces conf)
            , (m, f) <- [(noModMask, W.greedyView), (shiftMask, W.shift)]
            ]
            ++
            -- Function keys and symbols
            [ ((superMask, xK_F1), return ())
            , ((superMask, xK_F2), return ())
            , ((superMask, xK_F3), return ())
            , ((superMask, xK_F4), return ())
            , ((superMask, xK_F5), return ())
            , ((superMask, xK_F6), return ())
            , ((superMask, xK_F7), return ())
            , ((superMask, xK_F8), return ())
            , ((superMask, xK_F9), return ())
            , ((superMask, xK_F10), return ())
            , ((superMask, xK_F11), return ())
            , ((superMask, xK_F12), return ())
            , ((superMask, xK_Print), return ())
            , ((superMask, xK_Delete), return ())
            , ((noModMask, xF86XK_AudioMute), spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
            , ((noModMask, xF86XK_AudioLowerVolume), spawn "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%-")
            , ((noModMask, xF86XK_AudioRaiseVolume), spawn "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+")
            , ((noModMask, xF86XK_AudioPrev), spawn "playerctl previous")
            , ((noModMask, xF86XK_AudioPlay), spawn "playerctl play-pause")
            , ((noModMask, xF86XK_AudioNext), spawn "playerctl next")
            , ((noModMask, xF86XK_MonBrightnessDown), spawn "light -U 5")
            , ((noModMask, xF86XK_MonBrightnessUp), spawn "light -A 5")
            , ((superMask, xK_grave), namedScratchpadAction myScratchpads "terminal")
            , ((superMask, xK_Tab), windows W.focusDown) -- focus the next window (windows compatible)
            , ((superMask, xK_Return), spawn myTerminal) -- run terminal
            , ((superMask, xK_space), withFocused toggleFloat) -- toggle floating status
            ]
            ++
            -- Arrow keys
            -- super + arrow keys to move focus
            -- super + shift + arrow keys to move window
            [ ((m, k), f d False)
            | (k, d) <-
                [ (xK_k, U)
                , (xK_j, D)
                , (xK_h, L)
                , (xK_l, R)
                , (xK_Up, U)
                , (xK_Down, D)
                , (xK_Left, L)
                , (xK_Right, R)
                ]
            , (m, f) <-
                [ (superMask, windowGo)
                , (superMask .|. shiftMask, windowSwap)
                ]
            ]
            ++
            -- ctrl + super + left/right to view previous or next workspace
            [ ((controlMask .|. superMask, xK_Right), nextWS)
            , ((controlMask .|. superMask, xK_l), nextWS)
            , ((controlMask .|. superMask, xK_Left), prevWS)
            , ((controlMask .|. superMask, xK_h), prevWS)
            ]

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings _ =
    M.fromList
        [ ((superMask, leftClick), myDragWindow) -- move window
        , ((superMask, middleClick), doNothing)
        , ((superMask, rightClick), myResizeWindow) -- resize window
        , ((superMask, scrollUp), doNothing)
        , ((superMask, scrollDown), doNothing)
        ]
  where
    leftClick = button1
    middleClick = button2
    rightClick = button3
    scrollUp = button4
    scrollDown = button5
    doNothing _ = return ()

-- ---------------------------------------------------------------------
-- Layout hook
myLayoutHook =
    avoidStruts
        $ draggingVisualizer
        $ toggleLayouts Full
            . onWorkspace "code" (tiled (7 / 10))
            . onWorkspace "chat" fullscreen
            . onWorkspace "media" fullscreen
        $ bsp
  where
    equalGaps s =
        gaps
            [ (U, s)
            , (D, s)
            , (L, s)
            , (R, s)
            ]
    bsp =
        equalGaps myGapSize $ spacing myGapSize $ borderResize emptyBSP
    tiled frac =
        equalGaps myGapSize $ spacing myGapSize $ Mirror $ Tall 1 (2 / 100) frac
    fullscreen =
        equalGaps (myGapSize * 2) Full

-- ---------------------------------------------------------------------
-- Manage hook
myManageHook =
    composeAll
        [ namedScratchpadManageHook myScratchpads
        ]

-- ---------------------------------------------------------------------
-- Autostart programs
myStartupHook = do
    -- Audio daemon
    spawnOnce "pipewire"
    spawnOnce "wireplumber"
    spawnOnce "pipewire -c pipewire-pulse.conf"

    -- Eye candies
    spawnOnce "picom"
    spawnOnce "xwallpaper --daemon --zoom $HOME/Pictures/Wallpapers/6th-destroyer-division.jpg"

    -- Utilities
    spawnOnce "eww daemon"
    spawnOnce "emacs --fg-daemon"

-- ---------------------------------------------------------------------
-- Entrypoint
main :: IO ()
main =
    xmonad $
        withNavigation2DConfig def $
            desktopConfig
                { borderWidth = myBorderWidth
                , focusFollowsMouse = myFocusFollowsMouse
                , focusedBorderColor = myFocusedBorderColor
                , keys = myKeys
                , layoutHook = myLayoutHook
                , manageHook = myManageHook
                , mouseBindings = myMouseBindings
                , normalBorderColor = myNormalBorderColor
                , startupHook = myStartupHook
                , workspaces = myWorkspaces
                }
