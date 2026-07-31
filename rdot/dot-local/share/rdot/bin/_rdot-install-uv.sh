#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


remote=$1; shift

aloud-bracketed ssh -T "$remote" -- bash - <<EOF
$(cat "$script_real_dir"/prelude.sh)
aloud curl -LsSf https://astral.sh/uv/install.sh | sh
EOF
status=$?; test $status == 0 || exit $status
log 'success> Done.' # Clearly mark the script finished correctly.
