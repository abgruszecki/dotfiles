#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


repo_name=$1
remote=$2

aloud-bracketed ssh -T "$remote" -- bash - <<EOF
$(cat "$script_real_dir"/prelude.sh)
aloud cd && aloud cd "$repo_name" || exit
aloud git switch dev || exit
aloud git add --all || exit
log 'info> Checking if there is anything to commit.'
! aloud git diff-index --quiet @ || exit 0
ts=$(date +%Y%m%dT%H%M) # this is the local date
aloud git commit -m "$ts -- $remote" || exit
aloud git status --porcelain
EOF
