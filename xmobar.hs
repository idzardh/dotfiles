-- TODO: mpd args refresh rate!
Config { font    = "xft:UbuntuMonoDerivativePowerline Nerd Font Regular:size=9"
       , additionalFonts = ["xft:UbuntuMonoDerivativePowerline Nerd Font:size=10"]
       , bgColor = "#000000" --"#392737"
       , fgColor = "#2c8fa0" --"grey"
       , alpha   = 220
       , allDesktops = True
       , position = TopW L 95
       , commands = [
                      Run Cpu
                        [ "-t", "<fn=1>\xf085</fn> <vbar>"
                        , "-L", "2"
                        , "-H", "60"
                        , "-l", "#586e75"
                        , "-n", "green"
                        , "-h", "red"
                        ] 10
                    , Run Memory
                        [ "-t", "<fn=1>\xf473</fn> <usedvbar>"
                        , "-p", "2"
                        , "-l", "#586e75"
                        , "-h", "#268bd2"
                        ] 10
                    , Run Date "<fn=1>\xf073</fn> %a %b %_d %Y | %H:%M:%S" "date" 10
                    , Run Com "/usr/bin/zsh" ["-c", "~/dotfiles/scripts/volume.sh"] "vol" 10
                    , Run Com "/usr/bin/zsh" ["-c", "~/dotfiles/scripts/todo.sh"] "todo" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% | %memory% | <fn=1></fn> %todo% | %vol% | %date%"
       }
