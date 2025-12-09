#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


remote=$1; shift

aloud-bracketed ssh -T "$remote" -- bash - <<EOF
$(cat "$script_real_dir"/prelude.sh)
aloud mkdir -p ~/.local || exit
aloud cd ~/.local || exit
aloud mkdir -p ./{bin,my-apps,my-stow} || exit

aloud rm -rf my-apps/nvim || exit
aloud mkdir -p my-apps/nvim || exit

aloud cd my-apps/nvim || exit
log "Piping: curl | tar"
aloud curl -sSfL https://github.com/neovim/neovim/releases/download/v0.11.5/nvim-linux-x86_64.tar.gz | aloud tar xzf -
[[ $? == 0 ]] || exit
aloud cd ~/.local || exit

aloud rm -rf my-stow/nvim/bin || exit
aloud mkdir -p my-stow/nvim/bin || exit
aloud ln -srv my-apps/nvim/nvim-linux-x86_64/bin/nvim my-stow/nvim/bin/nvim || exit
aloud ln -srv my-stow/nvim/bin/nvim my-stow/nvim/bin/vim || exit
aloud cd my-stow  || exit
aloud stow -v -S nvim || exit
EOF
status=$?; test $status == 0 || exit $status
log 'success> Done.' # Clearly mark the script finished correctly.
