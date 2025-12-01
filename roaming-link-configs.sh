#!/usr/bin/env bash
log() {
    echo >&2 "$@"
}

aloud() {
    { printf '$'; printf ' %q' "$@"; echo; } >&2
    "$@"
}


cd ~ || exit
test -d dotfiles || {
    echo >&2 'Exiting - missing dir : ~/dotfiles/'
    exit 1
}
aloud ln -sf dotfiles/tmux/dot-config/tmux/tmux.conf .tmux.conf
aloud ln -sf dotfiles/bash--universal/dot-bash.d .bash.d
