#!/usr/bin/env bash

aloud() {
    { printf '$' && printf ' %q' "$@" && printf '\n'; } >&2
    "$@"
}


machine_name=$1

aloud ln -sf "$HOME/dotfiles-private/$1/bash-local" "$HOME/.bash.d/local"
