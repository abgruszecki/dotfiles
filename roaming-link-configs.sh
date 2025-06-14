#!/usr/bin/env sh

cd ~
test -e dotfiles || {
    echo >&2 'Missing dir, exiting: ~/dotfiles/'
    exit 1
}
ln -sf dotfiles/tmux/.config/tmux/tmux.conf .tmux.conf
ln -sf dotfiles/bash/.bash.d .bash.d
