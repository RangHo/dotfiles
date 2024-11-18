{-# LANGUAGE FlexibleInstances #-}

import Control.Concurrent
import Data.Char
import Data.List
import System.IO
import System.Process

import Xmobar

-- ---------------------------------------------------------------------
-- Xmobar format string helpers

wrap :: String -> String -> String -> String
wrap l r m = l ++ m ++ r

action :: String -> String -> String -> String
action command button =
    wrap
        ("<action=" ++ command ++ " button=" ++ button ++ ">")
        "</action>"

command :: String -> String
command = wrap "%" "%"

fontColor :: String -> String -> String
fontColor color = wrap ("<fc=" ++ color ++ ">") "</fc>"

fontIndex :: Int -> String -> String
fontIndex i = wrap ("<fn=" ++ show i ++ ">") "</fn>"

hspace :: Int -> String
hspace = wrap "<hspace=" "/>" . show

icon :: String -> String
icon = wrap "<icon=" "/>"

-- ---------------------------------------------------------------------
-- User-defined plugins

instance Show (String -> String) where
    showsPrec _ f = showString "<function>"

instance Read (String -> String) where
    readsPrec _ s = [(id, s)]

data PostprocessingCommandReader = PostprocessingCommandReader String (String -> String) String
    deriving (Read, Show)

instance Exec PostprocessingCommandReader where
    alias (PostprocessingCommandReader _ _ a) = a
    start (PostprocessingCommandReader c f _) cb = do
        (hIn, hOut, hErr, hProc) <- runInteractiveCommand c
        hClose hIn
        hClose hErr
        hSetBinaryMode hOut False
        hSetBuffering hOut LineBuffering
        go hProc $ hGetLine hOut >>= (cb . f)
      where
        go p m = do
            m -- display the current line
            e <- getProcessExitCode p
            case e of
                Nothing -> go p m -- not finished, keep going
                Just _ -> cb "EXITED" -- finished, notify the callback

-- ---------------------------------------------------------------------
-- Main configuration

config :: Config
config =
    defaultConfig
        { font = "NeoDunggeunmo Pro 12"
        , textOffset = 2
        , additionalFonts =
            [ "Noto Sans Mono CJK KR 12"
            , "Noto Sans Mono CJK JP 12"
            , "Noto Sans Mono CJK SC 12"
            , "Noto Sans Mono CJK TC 12"
            ]
        , textOffsets =
            [ 0
            , 0
            , 0
            , 0
            ]
        , iconRoot = ".config/xmonad/icons"
        , iconOffset = 0
        , bgColor = "black"
        , fgColor = "white"
        , alpha = 128
        , position = TopHM 40 20 20 20 0
        , commands =
            [ Run $
                Alsa
                    "default"
                    "Master"
                    [ "-t"
                    , "<status> <volume>%"
                    , "--"
                    , "-O"
                    , "<volumeipat>"
                    , "-o"
                    , icon "volume-x.xpm"
                    , "--volume-icon-pattern"
                    , icon "volume-%%.xpm"
                    ]
            , Run $
                Battery
                    [ "-t"
                    , "<leftipat> <left>%"
                    , "--"
                    , "--on-icon-pattern"
                    , icon "battery-charging.xpm"
                    , "--idle-icon-pattern"
                    , icon "battery-charging.xpm"
                    , "--off-icon-pattern"
                    , icon "battery-%%.xpm"
                    ]
                    100
            , Run $
                Date (icon "clock.xpm" ++ " %H:%M") "clock" 10
            , Run $
                PostprocessingCommandReader
                    "echo && playerctl metadata --follow --format '{{default(artist, \"???\")}} - {{default(title, \"???\")}}'"
                    ( (icon "music.xpm" ++)
                        . (" " ++)
                        . fontIndex 1
                        . (\s -> if null s then "*cricket noises*" else s)
                    )
                    "music"
            , Run UnsafeXMonadLog
            , Run $
                Wireless
                    ""
                    [ "-t"
                    , "<qualityipat> <essid>"
                    , "--"
                    , "--quality-icon-pattern"
                    , icon "radio-%%.xpm"
                    ]
                    100
            ]
        , template =
            unwords
                [ hspace 20
                , command "UnsafeXMonadLog"
                , "}"
                , command "music"
                , sep
                , command "alsa:default:Master"
                , hspace 20
                , "{"
                , command "wi"
                , sep
                , command "battery"
                , sep
                , command "clock"
                , sep
                , action "rofi" "1" $ icon "power.xpm"
                , hspace 20
                ]
        }
  where
    sep = hspace 10 ++ fontColor "gray" "|" ++ hspace 10

main :: IO ()
main = do
    threadDelay 1000000
    xmobar config
