#!/usr/bin/env bash
script_real_dir=$(dirname "$(realpath "$0")")

log() {
    echo >&2 "$@"
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
