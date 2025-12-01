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

aloud mkdir -p .config/tmux
aloud ln -sf dotfiles/tmux/dot-config/tmux/tmux.conf .config/tmux/

aloud rm -f .bash.d # remove the symlink if it exists
aloud ln -sf dotfiles/bash--universal/dot-bash.d .bash.d
