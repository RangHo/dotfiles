module Backlight (
    getBrightness,
    setBrightness,
    incBrightness,
    decBrightness,
    primaryDriver,
) where

import Data.List
import Data.Maybe
import Data.Tuple
import System.Directory
import System.FilePath
import System.IO
import System.IO.Strict as SIO

-- | The path to the backlight sysfs directory.
sysfsBacklightPath :: FilePath
sysfsBacklightPath = "/sys/class/backlight"

-- | Get the path to the brightness file for a given backlight driver.
brightness :: String -> FilePath
brightness driver = sysfsBacklightPath </> driver </> "brightness"

-- | Get the path to the max_brightness file for a given backlight driver.
maxBrightness :: String -> FilePath
maxBrightness driver = sysfsBacklightPath </> driver </> "max_brightness"

-- | Get the list of all backlight drivers.
getAllDrivers :: IO [String]
getAllDrivers = listDirectory sysfsBacklightPath

-- | The primary driver available on the system.
primaryDriver :: IO String
primaryDriver = head <$> getAllDrivers

-- | Get the current raw brightness level for a given driver.
getRawBrightness :: String -> IO Int
getRawBrightness driver = read <$> SIO.readFile (brightness driver)

-- | Get the maximum raw brightness level for a given driver.
getMaxRawBrightness :: String -> IO Int
getMaxRawBrightness driver = read <$> SIO.readFile (maxBrightness driver)

-- | Set the raw brightness level for a given driver.
setRawBrightness :: String -> Int -> IO ()
setRawBrightness driver level = writeFile (brightness driver) $ show level

-- | Convert the perceived brightness value to the actual brightness value.
toActualBrightness ::
    -- | Maximum brightness value of the driver.
    Int ->
    -- | Perceived brightness value, in percentage (i.e. within [0, 100] range).
    Int ->
    -- | Converted actual brightness value.
    Int
toActualBrightness max value = round $ fromIntegral max * (fromIntegral value / 100.0) ** (1 / 2)

-- | Convert the actual brightness value to the perceived brightness value.
toPerceivedBrightness ::
    -- | Maximum brightenss value of the driver.
    Int ->
    -- | Actual brightness value.
    Int ->
    -- | Converted perceived brightness value.
    Int
toPerceivedBrightness max value = round $ 100.0 * (fromIntegral value / fromIntegral max) ** 2

-- | The number of steps to use for the brightness values.
brightnessSteps :: Int
brightnessSteps = 10

-- | The list of brightness values to use.
brightnessValues :: [Double]
brightnessValues = do
    i <- [0 .. brightnessSteps]
    let d = fromIntegral i / fromIntegral brightnessSteps
    return $ d ** 2

-- | Get the normalized brightness values for a given driver.
brightnessValuesForDriver :: String -> IO [Int]
brightnessValuesForDriver driver = do
    max <- fromIntegral <$> getMaxRawBrightness driver
    return $ map (round . (* max)) brightnessValues

-- | Get the normalized brightness level for a given driver.
getBrightness :: String -> IO Int
getBrightness driver = do
    max <- getMaxRawBrightness driver
    val <- getRawBrightness driver
    return $ toPerceivedBrightness max val

-- | Set the normalized brightness level for a given driver.
setBrightness :: String -> Int -> IO ()
setBrightness driver value = do
    max <- getMaxRawBrightness driver
    setRawBrightness driver $ toActualBrightness max value

-- | Increase the normalized brightness level to the next higher value.
incBrightness :: String -> IO ()
incBrightness driver = do
    values <- brightnessValuesForDriver driver
    before <- getRawBrightness driver
    let rest = dropWhile (<= fromIntegral before) values
    case rest of
        (x : _) -> setRawBrightness driver x
        [] -> return ()

-- | Decrease the normalized brightness level to the next lower value.
decBrightness :: String -> IO ()
decBrightness driver = do
    values <- brightnessValuesForDriver driver
    before <- getRawBrightness driver
    let rest = dropWhile (>= before) $ reverse values
    case rest of
        (x : _) -> setRawBrightness driver x
        [] -> return ()
