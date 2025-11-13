#!/usr/bin/env bash

log() {
    echo >&2 "$@"
}

aloud() {
    { printf '$'; printf ' %q' "$@"; printf '\n'; } >&2
    "$@"
}


repo=$1
remote=$2

local_repo_dir=~/"$repo"
remote_repo_dir="~/$repo"


log Checking if git remote exists: "$remote"
git -C "$local_repo_dir" remote -v | cut -f1 | grep "$remote" > /dev/null
ret=$?
test $ret == 0 || {
    log git remote not found: "$remote"
    log git remotes:
    git -C "$local_repo_dir" remote -v | cut -f1 | sort | uniq
    # Is there any reason not to run this automatically?
    # The script anyway doesn't support anything but the default dir.
    log Consider running the following command.
    log "git -C $local_repo_dir remote add $remote ssh://$remote/$remote_repo_dir"
    exit $ret
}


aloud ssh "$remote" mkdir "$remote_repo_dir" '&&' git -C "$remote_repo_dir" init || exit
aloud git -C "$local_repo_dir" push "$remote" dev || exit
aloud ssh "$remote" git -C "$remote_repo_dir" checkout dev || exit
aloud git -C "$local_repo_dir" push "$remote" master || exit
