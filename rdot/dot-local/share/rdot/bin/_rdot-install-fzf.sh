#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


remote=$1; shift

aloud-bracketed ssh -T "$remote" -- bash - <<EOF
$(cat "$script_real_dir"/prelude.sh)
aloud mkdir -p ~/.local/share/ || exit
aloud cd ~/.local/share/ || exit

aloud git clone --depth 1 https://github.com/junegunn/fzf.git || exit
aloud fzf/install --bin || exit
aloud ln -vrs fzf/bin/fzf ../bin/ || exit
EOF
