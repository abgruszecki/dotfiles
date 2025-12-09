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

# NOTE:
# This really screwed up the directories and made me fix remotes for hours.
# If the target of ln is a symlinked directory it's treated as a dir.
# This created a lot of unnecessary directories in random places.
# I really should rely on `stow` to manage symlinks,
# and only symlink things manually if there's no other option.

# aloud mkdir -p .config/tmux
# aloud ln -srv dotfiles/tmux/dot-config/tmux/tmux.conf .config/tmux/
# 
# aloud rm -f .bash.d # remove the symlink if it exists
# aloud ln -srv dotfiles/bash--universal/dot-bash.d .bash.d
