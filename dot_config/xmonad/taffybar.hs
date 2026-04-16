import Data.Default
import Data.Text
import Graphics.UI.GIGtkStrut
import System.Environment.XDG.BaseDir
import System.IO
import System.Log.Logger

import System.Taffybar
import qualified System.Taffybar.Context as Ctx
import System.Taffybar.DBus
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph

-- ---------------------------------------------------------------------
-- Helper functions

-- | Make RGBA color from 0-256 range to 0-1 range.
mkRGBA (r, g, b, a) = (r / 256, g / 256, b / 256, a / 256)

-- ---------------------------------------------------------------------
-- Shared configurations

blue = mkRGBA (42, 99, 140, 256)

yellow1 = mkRGBA (242, 163, 54, 256)

yellow2 = mkRGBA (254, 204, 83, 256)

yellow3 = mkRGBA (227, 134, 18, 256)

red = mkRGBA (210, 77, 37, 256)

myGraphConfig :: GraphConfig
myGraphConfig =
    def
        { graphPadding = 0
        , graphBorderWidth = 0
        , graphWidth = 50
        , graphBackgroundColor = (0, 0, 0, 0)
        }

-- ---------------------------------------------------------------------
-- Widgets

myBattery =
    batteryIconNew

myClock =
    textClockNewWith
        def
            { clockUpdateStrategy = RoundedTargetInterval 60 0.0
            , clockFormatString = "%a %b %_d, %I:%M %p"
            }

myCpuGraph =
    pollingGraphNew graphConfig 5 action
  where
    graphConfig =
        myGraphConfig
            { graphDataColors = [red, (1, 0, 1, 0.5)]
            , graphLabel = Just $ pack "cpu"
            }
    action = do
        (_, systemLoad, totalLoad) <- cpuLoad
        return [totalLoad, systemLoad]

myMpris2 =
    mpris2New

mySniTray =
    sniTrayNew

myWorkspaces =
    workspacesNew def

myConfig :: SimpleTaffyConfig
myConfig =
    def
        { monitorsAction = useAllMonitors
        , barPosition = Top
        , barHeight = ExactSize 40
        , barPadding = 20
        , startWidgets =
            [ myWorkspaces
            ]
        , centerWidgets =
            [ myMpris2
            ]
        , endWidgets =
            [ myClock
            , myCpuGraph
            , myBattery
            , mySniTray
            ]
        }

main :: IO ()
main = do
    taffybarCss <- getUserConfigFile "xmonad" "taffybar.css"
    startTaffybar
        . withLogServer
        $ toTaffybarConfig myConfig{cssPaths = [taffybarCss]}
