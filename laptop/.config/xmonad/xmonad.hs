import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Default
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Graphics.X11.ExtraTypes.XF86
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Process

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize as FR
import XMonad.Actions.Navigation2D
import XMonad.Actions.TiledWindowDragging
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.Gaps
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Hacks
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

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
-- Applications and commands

myBar = "~/.cache/xmonad/taffybar"
myBrowser = "firefox"
myDisplaySettings = "xfce4-display-settings"
myEditor = "emacsclient -c"
myEmailClient = "thunderbird"
myFileExplorer = "Thunar"
myLauncher = "rofi -show drun"
myScreenshot = "xfce4-screenshooter"

-- ---------------------------------------------------------------------
-- Helper functions

-- | Find the index of a workspace by its name.
findWorkspaceIndex :: WorkspaceId -> Int
findWorkspaceIndex ws = fromMaybe 0 $ elemIndex ws myWorkspaces

-- | Check if a window is in floating state.
isFloating :: Window -> WindowSet -> Bool
isFloating w ws = M.member w (W.floating ws)

-- ---------------------------------------------------------------------
-- Screen brightness utility

type BrightnessDriver = FilePath

sysfsBacklightPath :: FilePath
sysfsBacklightPath = "/sys/class/backlight"

mkBrightnessDriver :: String -> BrightnessDriver
mkBrightnessDriver = (</>) sysfsBacklightPath

lsBrightnessDrivers :: IO [BrightnessDriver]
lsBrightnessDrivers = listDirectory sysfsBacklightPath >>= mapM (return . mkBrightnessDriver)

mainBrightnessDriver :: IO BrightnessDriver
mainBrightnessDriver = head <$> lsBrightnessDrivers

readBrightnessFile :: BrightnessDriver -> IO (Maybe Int)
readBrightnessFile f = do
    content <- B.readFile f
    case B.readInt content of
        Just (v, _) -> return $ Just v
        _ -> return Nothing

getRawBrightness :: BrightnessDriver -> IO (Maybe Int)
getRawBrightness = readBrightnessFile . (</> "brightness")

setRawBrightness :: BrightnessDriver -> Int -> IO ()
setRawBrightness drv value = B.writeFile (drv </> "brightness") (B.pack $ show value)

getRawMaxBrightness :: BrightnessDriver -> IO (Maybe Int)
getRawMaxBrightness drv = readBrightnessFile (drv </> "max_brightness")

getBrightness :: BrightnessDriver -> IO Double
getBrightness drv = do
    val <- fromIntegral . fromMaybe 0 <$> getRawBrightness drv
    max <- fromIntegral . fromMaybe 0 <$> getRawMaxBrightness drv
    let gamma = (val / max) ** (1 / 2)
    return gamma

setBrightness :: BrightnessDriver -> Double -> IO ()
setBrightness drv val = do
    max <- fromIntegral . fromMaybe 0 <$> getRawMaxBrightness drv
    let raw = round $ max * val ** 2
    setRawBrightness drv raw

getMainBrightness :: IO Double
getMainBrightness = do
    drv <- mainBrightnessDriver
    getBrightness drv

setMainBrightness :: Double -> IO ()
setMainBrightness val = do
    drv <- mainBrightnessDriver
    setBrightness drv val

incBrightness :: Double -> IO ()
incBrightness delta = do
    oldVal <- getMainBrightness
    let newVal = min 1.0 (oldVal + delta)
    setMainBrightness newVal

decBrightness :: Double -> IO ()
decBrightness delta = do
    oldVal <- getMainBrightness
    let newVal = max 0.0 (oldVal - delta)
    setMainBrightness newVal

-- ---------------------------------------------------------------------
-- Key bindings

-- | Drag the window with the mouse, depending on the floating state.
dragWindowOrFloating :: Window -> X ()
dragWindowOrFloating w =
    let dragFloating w = focus w >> mouseMoveWindow w >> windows W.shiftMaster
     in do
            ws <- gets windowset
            if isFloating w ws
                then dragFloating w
                else dragWindow w

-- | Resize the window with the mouse, depending on the floating state.
resizeWindowWhenFloating :: Window -> X ()
resizeWindowWhenFloating w =
    let resizeFloating = focus w >> FR.mouseResizeWindow w >> windows W.shiftMaster
     in do
            ws <- gets windowset
            when (isFloating w ws) resizeFloating

-- | Toggle the floating state of a window.
toggleFloat :: Window -> X ()
toggleFloat w =
    let toggle w ws =
            if isFloating w ws
                then W.sink w ws
                else W.float w myDefaultFloatRect ws
     in windows $ toggle w

-- | Toggle fullscreen mode.
toggleFullscreen :: Window -> X ()
toggleFullscreen _ = do
    sendMessage (Toggle "Full")
    sendMessage ToggleStruts

-- | Power off the system.
powerOff :: X ()
powerOff = do
    -- Kill all windows first.
    ws <- gets windowset
    mapM_ killWindow (W.allWindows ws)
    -- Run other custom shutdown commands.
    spawn "emacsclient -e '(kill-emacs)'"
    -- Call poweroff command as root.
    spawn "sleep 1; sudo poweroff"

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf =
    M.fromList $
        -- XMonad management
        [ ((controlMask .|. altMask, xK_Delete), powerOff) -- Ctrl + Alt + Delete to show power menu
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
            , ((superMask, xK_p), spawn myDisplaySettings) -- run display settings (windows compatible)
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
            , ((noModMask, xF86XK_MonBrightnessDown), io $ decBrightness 0.05)
            , ((noModMask, xF86XK_MonBrightnessUp), io $ incBrightness 0.05)
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
        [ ((superMask, leftClick), dragWindowOrFloating) -- move window
        , ((superMask, middleClick), doNothing)
        , ((superMask, rightClick), resizeWindowWhenFloating) -- resize window
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
    beardGaps s =
        gaps
            [ (U, 0)
            , (D, s)
            , (L, s)
            , (R, s)
            ]
    bsp =
        beardGaps myGapSize $ spacing myGapSize $ borderResize emptyBSP
    tiled frac =
        beardGaps myGapSize $ spacing myGapSize $ Mirror $ Tall 1 (2 / 100) frac
    fullscreen =
        beardGaps (myGapSize * 2) Full

-- ---------------------------------------------------------------------
-- Manage hook

-- | Copy the managed window to all workspaces.
doCopyToAll :: ManageHook
doCopyToAll = doF copyToAll

myManageHook =
    composeAll
        [ namedScratchpadManageHook myScratchpads
        , isDialog --> doCenterFloat
        , stringProperty "WM_WINDOW_ROLE" =? "PictureInPicture" --> (doRectFloat myPIPRect <+> doCopyToAll)
        ]

-- ---------------------------------------------------------------------
-- Autostart programs

myStartupHook = do
    -- Daemons
    spawnOnce "status-notifier-watcher"
    spawnOnce "emacs --fg-daemon"

    -- Eye candies
    spawnOnce "picom"
    spawnOnce "wal -R"

    -- Utilities
    spawnOnce "~/.cache/xmonad/taffybar"
    spawnOnce "dunst"

-- ---------------------------------------------------------------------
-- Entrypoint

main :: IO ()
main = do
    xmonad
        . ewmh
        . ewmhFullscreen
        . docks
        . javaHack
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
