# Dotfiles

This repository contains the dotfiles for various programs I use. Although they are not very heavily commented I hope they can be of use to some people.

I currently use XMonad (xmonad.org) with polybar (https://github.com/jaagr/polybar). For editing and coding I use vim (dotfiles can be found below) and tmux.

Some screenshots of my system can be found here: https://idzardblog.wordpress.com/2017/09/17/xmonad-polybar/

## Contents

.config/polybar:
* config                   : the polybar configuration file
* launch.sh                : very simple script that is used for (re-)starting polybar from within xmonad

scripts:
* extrasuperkeys           : use the tab key as a second/third mod4mask key for xmonad to simplify a lot of key combinations
* lockscreen.sh / lock.png : takes a screenshot and distorts it using imagemagick's convert function
* todo.sh                  : grabs the todo list and counts the number of entries, temporary using calcurse, see if it works
* volume.sh                : grabs the current volume and displays an icon to reflect the volume
* updates.sh               : very simple script that shows how many packages can be updated

zsh:
* .zshrc : some basic stuff for configuring the command line to print some useful information, the other files are used as part of the git displaying in the command line, might replace it with parts of oh-my-zsh in the future

globalvim.vim : my vim settings

xmonad.hs : my xmonad configuration

