#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


repo=$1
remote=$2

local_repo_dir=~/"$repo"
remote_repo_dir_template="\$HOME/$repo"


aloud cd "$local_repo_dir" || exit
remote_repo_dir=$(aloud ssh "$remote" echo "$remote_repo_dir_template")
ret=$?
test $ret == 0 || {
    log Failed to resolve remote repo dir.
    exit $ret
}
log Resolved remote repo dir: "$remote_repo_dir"

remote_repo_git_target=$(printf 'ssh://%s/%s' "$remote" "$remote_repo_dir")
git_remote_row=$(git remote -v | grep -F "(fetch)" | grep "^$remote")
if test $? == 0; then
    test "$(echo "$git_remote_row" | awk '{ print $2 }')" == "$remote_repo_git_target" || {
        log git remote found, but has an unexpected target
        log git remote -v prints: "$git_remote_row"
        exit 1
    }
else
    log git remote not found: "$remote"
    aloud git -C "$local_repo_dir" remote add "$remote" "$remote_repo_git_target" || exit
fi


# Notes.
# Currently more-or-less the following manual steps are also needed.
# The last step doesn't exactly work on all systems,
# there are some issues with the config files present on systems like Ubuntu.
# $ ~/dotfiles/roaming-link-configs.sh
# $ ~/.bash.d/link-local.sh $remote
# $ echo '. ~/.bash.d/source-all.sh' >> ~/.bashrc
aloud ssh "$remote" mkdir "$remote_repo_dir" '&&' git -C "$remote_repo_dir" init || exit
aloud git push "$remote" dev || exit
aloud ssh "$remote" git -C "$remote_repo_dir" checkout dev || exit
aloud git push "$remote" master || exit
