#!/usr/bin/env bash
xmodmap /home/gruszecki/dotfiles/capslock.d/xmodmaprc

killall xcape
xcape -t 500 -e "Hyper_L=Escape;Shift_L=parenleft;Shift_R=parenright"
