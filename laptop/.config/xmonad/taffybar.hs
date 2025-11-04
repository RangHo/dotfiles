import Data.Default
import Graphics.UI.GIGtkStrut
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
-- Shared configurations

myGraphConfig :: GraphConfig
myGraphConfig =
    def

-- ---------------------------------------------------------------------
-- Widgets

myBattery =
    batteryIconNew

myClock =
    textClockNewWith
        def
            { clockFormatString = "%Y-%m-%d %I:%M %p"
            }

myMPRIS2 =
    mpris2New

mySNITray =
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
            [ myMPRIS2
            ]
        , endWidgets =
            [ myClock
            , myBattery
            , mySNITray
            ]
        }

main :: IO ()
main = do
    startTaffybar
        . withLogServer
        $ toTaffybarConfig myConfig
