#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


version=6.20.2
archive_dir=miller-$version-linux-amd64
url=https://github.com/johnkerl/miller/releases/download/v$version/$archive_dir.tar.gz


remote=$1; shift

aloud-bracketed ssh -T "$remote" -- bash - <<EOF
$(cat "$script_real_dir"/prelude.sh)
aloud mkdir -p ~/.local || exit
aloud cd ~/.local || exit
aloud mkdir -p ./{bin,my-apps,my-stow} || exit

aloud rm -rf my-apps/mlr || exit
aloud mkdir -p my-apps/mlr || exit

aloud cd my-apps/mlr || exit
log "Piping: curl | tar"
aloud curl -sSfL "$url" | aloud tar xzf -
[[ \$? == 0 ]] || exit
aloud cd ~/.local || exit

aloud rm -rf my-stow/mlr/bin || exit
aloud mkdir -p my-stow/mlr/bin || exit
aloud ln -srv my-apps/mlr/$archive_dir/mlr my-stow/mlr/bin/mlr || exit
aloud cd my-stow  || exit
aloud stow -v -S mlr || exit
EOF
status=$?; test $status == 0 || exit $status
log 'success> Done.' # Clearly mark the script finished correctly.
