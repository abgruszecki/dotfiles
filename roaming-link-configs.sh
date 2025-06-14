#!/usr/bin/env sh

cd ~
test -e dotfiles || {
    echo >&2 'Missing dir, exiting: ~/dotfiles/'
    exit 1
}
ln -s dotfiles/tmux/.config/tmux/tmux.conf .tmux.conf
ln -s dotfiles/bash/.bash.d .bash.d
