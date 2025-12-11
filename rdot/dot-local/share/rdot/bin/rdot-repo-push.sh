#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


repo_name=$1
remote=$2

ensure-init || die 1 "ensure-init failed!"
aloud cd ~/"$repo_name" || exit
aloud git push "$remote" master --force-with-lease || exit
aloud-bracketed rdot-ssh -T "$remote" -- bash - <<EOF
$(cat "$script_real_dir"/prelude.sh)
aloud cd ~/"$repo_name" || exit
aloud git switch dev || exit
aloud git rebase --autostash master || exit
aloud git status --porcelain
EOF
status=$?; test $status == 0 || exit $status
log 'success> Done.' # Clearly mark the script finished correctly.
