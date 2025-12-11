,print1() {
   printf '%s\n' "$@"
}

,djs-jobs-fzf() {
    local dir=$1
    dir=${dir:-./jobs}
    ( shopt -s globstar; dirname "$dir"/**/job.sh ) | fzf -e
}
