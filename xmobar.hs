Config { -- font = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
         font    = "xft:UbuntuMonoDerivativePowerline Nerd Font Regular:size=9"
         --font    = "xft:Roboto Mono Nerd Font Complete:size=9"
       , additionalFonts = ["xft:UbuntuMonoDerivativePowerline Nerd Font:size=10"]
       , bgColor = "#392737"
       , fgColor = "grey"
       , alpha   = 140
       , position = Top
       , commands = [
                    --Run Network "enp30s0" ["-L","0","-H","32","--normal","green","--high","red"] 10
                     Run Cpu ["-L","15","-H","50","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% | %memory% | %date%"
       }
