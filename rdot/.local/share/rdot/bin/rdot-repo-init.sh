#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


repo=$1
remote=$2

local_repo_dir=~/"$repo"
remote_repo_dir="\$HOME/$repo"


aloud cd "$local_repo_dir" || exit
log Checking if git remote exists: "$remote"
git remote -v | cut -f1 | grep "$remote" > /dev/null
ret=$?
test $ret == 0 || {
    log git remote not found: "$remote"
    log git remotes:
    git remote -v | cut -f1 | sort | uniq
    # Is there any reason not to run this automatically?
    # The script anyway doesn't support anything but the default dir.
    log Consider running the following command.
    log "git -C \"$local_repo_dir\" remote add \"$remote\" \"ssh://$remote/$remote_repo_dir\""
    exit $ret
}


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
