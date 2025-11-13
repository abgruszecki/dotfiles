#!/usr/bin/env bash
log() {
    echo >&2 "$@"
}

aloud() {
    { printf '$'; printf ' %q' "$@"; printf '\n'; } >&2
    "$@"
}


cd ~ || exit
test -e dotfiles || {
    echo >&2 'Exiting, as dir is missing: ~/dotfiles/'
    exit 1
}
aloud ln -sf dotfiles/tmux/.config/tmux/tmux.conf .tmux.conf
aloud ln -sf dotfiles/bash/.bash.d .bash.d
