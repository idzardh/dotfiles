#!/bin/zsh
scrot /tmp/screen.png
convert /tmp/screen.png -paint 2 /tmp/screen.png
[[ -f ~/dotfiles/scripts/lock.png ]] && convert /tmp/screen.png ~/dotfiles/scripts/lock.png -gravity center -composite -matte /tmp/screen.png
i3lock -e -i /tmp/screen.png
