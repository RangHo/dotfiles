import Data.Char
import Data.List

import Xmobar

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
        , iconRoot = ".config/xmobar/icons"
        , iconOffset = 0
        , bgColor = "black"
        , fgColor = "white"
        , alpha = 128
        , position = TopHM 40 20 20 20 0
        , commands =
            [ Run UnsafeXMonadLog
            , Run $
                Date "<icon=clock.xpm/> %H:%M" "clock" 10
            , Run $
                Battery
                    ( args
                        [ "-t <leftipat> <left>%"
                        , "--"
                        , "--on-icon-pattern <icon=battery-charging.xpm/>"
                        , "--idle-icon-pattern <icon=battery-charging.xpm/>"
                        , "--off-icon-pattern <icon=battery-%%.xpm/>"
                        ]
                    )
                    100
            , Run $
                Mpris2
                    "spotify"
                    (args ["-t <icon=music.xpm/> <fn=1><artist> - <title></fn>"])
                    10
            , Run $
                Alsa
                    "default"
                    "Master"
                    ( args
                        [ "-t <status> <volume>%"
                        , "--"
                        , "-O <volumeipat> "
                        , "-o <icon=volume-x.xpm/> "
                        , "--volume-icon-pattern <icon=volume-%%.xpm/>"
                        ]
                    )
            , Run $
                Wireless
                    ""
                    ( args
                        [ "-t <qualityipat> <ssid>"
                        , "--"
                        , "--quality-icon-pattern <icon=radio-%%.xpm/>"
                        ]
                    )
                    100
            ]
        , template =
            unwords
                [ hspace 20
                , command "UnsafeXMonadLog"
                , "}"
                , command "mpris2"
                , hspace 20
                , "|"
                , hspace 20
                , command "alsa:default:Master"
                , hspace 20
                , "{"
                , command "wi"
                , sep
                , command "battery"
                , sep
                , command "clock"
                , hspace 20
                ]
        }
  where
    args =
        foldr
            ( \arg acc ->
                case words arg of
                    a : b -> a : unwords b : acc
                    [a] -> a : acc
            )
            []
    sep = hspace 10 ++ fontColor "gray" "┃┃" ++ hspace 10

main :: IO ()
main = xmobar config
