#!/usr/bin/env bash
. "$(dirname "$(realpath "$0")")"/prelude.sh || exit


url=https://github.com/Canop/broot/releases/download/v1.53.0/broot_1.53.0.zip


remote=$1; shift

aloud-bracketed ssh -T "$remote" -- bash - <<EOF
$(cat "$script_real_dir"/prelude.sh)
aloud mkdir -p ~/.local/share/broot || exit
tmpdir=\$(aloud mktemp -d) || exit
printf >&2 'tmpdir=%q\n' "\$tmpdir"
aloud wget -q -O "\$tmpdir/broot.zip" "$url" || exit
aloud cd ~/.local/share/broot || exit
aloud unzip -q "\$tmpdir/broot.zip" || exit
aloud ln -vrs ./x86_64-unknown-linux-musl/broot ~/.local/bin/ || exit
aloud ~/.local/bin/broot --set-install-state installed || exit
EOF
