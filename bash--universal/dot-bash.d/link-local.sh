#!/usr/bin/env bash
log() {
    echo >&2 "$@"
}

aloud() {
    { printf '$'; printf ' %q' "$@"; printf '\n'; } >&2
    "$@"
}


machine_name=$1

test ! -z "$machine_name" || {
    log Usage: "$0" "<machine-name>"
    exit 1
}


aloud ln -sf "$HOME/dotfiles-private/$1/bash-local" "$HOME/.bash.d/local"
