import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Graphics.X11.ExtraTypes.XF86
import System.Exit

import XMonad hiding (mouseResizeWindow)
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize
import XMonad.Actions.Navigation2D
import XMonad.Actions.TiledWindowDragging
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Gaps
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet as W
import XMonad.Util.Hacks
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

import Backlight

-- ---------------------------------------------------------------------
-- Constants
altMask = mod1Mask
superMask = mod4Mask

-- ---------------------------------------------------------------------
-- Variables
myBorderWidth = 0
myDefaultFloatRect = W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
myFocusFollowsMouse = True
myFocusedBorderColor = "#eeeeee"
myGapSize = 10
myModMask = superMask
myNormalBorderColor = "#222222"
myPIPRect = W.RationalRect (2 / 3) (2 / 3) (3 / 10) (3 / 10)
myScratchRect = W.RationalRect (1 / 8) (1 / 8) (3 / 4) (3 / 4)
myScratchpads =
    [ NS "terminal" "alacritty --class scratchpad" (className =? "scratchpad") focusedFloating
    ]
  where
    focusedFloating = customFloating myScratchRect
myTerminal = "alacritty"
myWorkspaces =
    [ "default"
    , "code"
    , "chat"
    , "media"
    , "misc"
    ]

-- ---------------------------------------------------------------------
-- Applications
myBrowser = "floorp"
myEditor = "emacs"
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
            else W.float w myDefaultFloatRect ws

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
        else dragWindow w
  where
    dragFloating w = do
        focus w
        mouseMoveWindow w
        windows W.shiftMaster

-- | Resize the window with the mouse, depending on the floating state.
myResizeWindow :: Window -> X ()
myResizeWindow w = do
    ws <- gets windowset
    when (isFloating w ws) $ do
        focus w
        mouseResizeWindow w
        windows W.shiftMaster

-- | Find the index of a workspace by its name.
findWorkspaceIndex :: WorkspaceId -> Int
findWorkspaceIndex ws = fromMaybe 0 $ elemIndex ws myWorkspaces

-- | Copy the managed window to all workspaces.
doCopyToAll :: ManageHook
doCopyToAll = doF copyToAll

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
            , ((superMask, xK_t), spawn myEditor) -- run [t]ext editor
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
            , ((noModMask, xF86XK_MonBrightnessDown), liftIO $ primaryDriver >>= decBrightness >> primaryDriver >>= getBrightness >>= putStrLn . ("Brightness: " ++) . show)
            , ((noModMask, xF86XK_MonBrightnessUp), liftIO $ primaryDriver >>= incBrightness >> primaryDriver >>= getBrightness >>= putStrLn . ("Brightness: " ++) . show)
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
-- Status bar
myPP =
    filterOutWsPP [scratchpadWorkspaceTag] $
        def
            { ppCurrent = gotoWorkspace $ xmobarColor "yellow" ""
            , ppVisible = gotoWorkspace $ xmobarColor "white" ""
            , ppHidden = gotoWorkspace $ xmobarColor "white" ""
            , ppHiddenNoWindows = gotoWorkspace $ xmobarColor "gray" ""
            , ppSep = " "
            , ppWsSep = xmobarColor "gray" "" " | "
            , ppOrder = \(ws : l : t : _) -> [ws]
            }
  where
    gotoWorkspace style wid =
        xmobarAction
            ("xdotool set_desktop " ++ show (findWorkspaceIndex wid))
            "1"
            (style wid)

mySB = statusBarProp "~/.cache/xmonad/xmobar" (pure myPP)

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
        , isDialog --> doCenterFloat
        , stringProperty "WM_WINDOW_ROLE" =? "PictureInPicture" --> (doRectFloat myPIPRect <+> doCopyToAll)
        ]

-- ---------------------------------------------------------------------
-- Autostart programs
myStartupHook = do
    -- Eye candies
    spawnOnce "picom"
    spawnOnce "wal -R"

    -- Utilities
    spawnOnce "dunst"
    spawnOnce "playerctld"

-- ---------------------------------------------------------------------
-- Entrypoint
main :: IO ()
main =
    xmonad
        . ewmh
        . ewmhFullscreen
        . docks
        . javaHack
        . withSB mySB
        . withNavigation2DConfig def
        $ def
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
