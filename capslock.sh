#!/usr/bin/env bash
xmodmap /home/gruszecki/dotfiles/capslock.d/xmodmaprc

killall xcape
# $ xev -event keyboard
xcape -t 200 -e "Hyper_L=Escape;Shift_L=parenleft;Shift_R=parenright"
