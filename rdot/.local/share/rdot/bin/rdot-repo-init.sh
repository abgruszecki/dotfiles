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


log Checking if git remote exists: "$remote"
git remote -v | cut -f1 | grep "$remote" > /dev/null
ret=$?
test $ret == 0 || {
    log git remote not found: "$remote"
    log git remotes:
    git remote -v | cut -f1 | sort | uniq
    exit $ret
}


aloud ssh "$remote" mkdir "$repo_dir" '&&' git -C "$repo_dir" init || exit
aloud git push "$remote" dev || exit
aloud ssh "$remote" git -C "$repo_dir" checkout dev || exit
aloud git push "$remote" master || exit
