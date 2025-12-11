#!/usr/bin/env bash
script_real_dir=$(dirname "$(realpath "$0")")

# This dir should anyway be created for other script users.
ssh_ctrlsock_dir=~/.ssh/.controlsocks--rdot

ssh_ctrlsock_args=(
    -o ControlMaster=auto
    -o ControlPath=$ssh_ctrlsock_dir/%C
    -o ControlPersist=1h
)


log() {
    echo >&2 "$@"
}

die() {
    exit_status=$1; shift
    log "$@"
    exit "$exit_status"
}

aloud() {
    { printf '$' && printf ' %q' "$@" && printf '\n'; } >&2
    "$@"
}

aloud-bracketed() {
    log 'info> +' "$@"
    "$@"
    ret=$?
    log 'info> -' "$@"
    return $ret
}

ensure-init() {
   mkdir -p "$ssh_ctrlsock_dir" 
}


rdot-ssh() {
    ssh "${ssh_ctrlsock_args[@]}" "$@"
}
