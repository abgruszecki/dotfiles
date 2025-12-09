#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


remote=$1; shift

aloud-bracketed ssh -T "$remote" -- bash - <<EOF
$(cat "$script_real_dir"/prelude.sh)
aloud mkdir -p ~/.local/my-apps || exit
aloud cd ~/.local/my-apps || exit
source_dir=stow-2.4.1
test -e \$source_dir && {
  log "Aborting - source dir already exists: \$source_dir"
  exit 1
}
log 'info> + curl | tar'
aloud curl https://ftp.gnu.org/gnu/stow/stow-2.4.1.tar.gz | aloud tar xzf -
log 'info> - curl | tar'
aloud cd \$source_dir || exit
aloud ./configure --prefix=\$HOME/.local/
aloud make install | tee install.log # says where things were installed
EOF
status=$?; test $status == 0 || exit $status
log 'success> Done.' # Clearly mark the script finished correctly.
