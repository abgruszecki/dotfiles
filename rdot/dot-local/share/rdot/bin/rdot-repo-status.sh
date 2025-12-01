#!/usr/bin/env bash

log() {
    echo >&2 "$@"
}

aloud() {
    { printf '$' && printf ' %q' "$@" && printf '\n'; } >&2
    "$@"
}


repo=$1
remote=$2

repo_dir="~/$repo"

aloud ssh "$remote" cd "$repo_dir" '&&' git status || exit
