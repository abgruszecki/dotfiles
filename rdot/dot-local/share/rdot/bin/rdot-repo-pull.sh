#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


aloud-bracketed rdot-remote-repo-commit.sh "$@" || exit
log '' # newline
aloud-bracketed rdot-repo-fetch-merge.sh "$@" || exit
log 'success> Done.' # Clearly mark the script finished correctly.
