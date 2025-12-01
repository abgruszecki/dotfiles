#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


repo=$1
remote=$2

aloud cd && aloud cd "$repo" || exit
aloud git fetch "$remote" || exit
aloud git merge --ff-only "$remote"/dev || exit
