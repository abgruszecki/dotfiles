,print1() {
   printf '%s\n' "$@"
}

,djs-jobs-fzf() {
    local dir=$1
    dir=${dir:-./jobs}
    ( shopt -s globstar; dirname "$dir"/**/job.sh ) | fzf -e
}

,dotenv-load() {
    local f=$1; shift
    [[ -f $f ]] || {
        echo >&2 ",dotenv-load: expected \$1 to be a file, was: $f"
        exit 1
    }
    set -a
    . "$f"
    set +a
}
