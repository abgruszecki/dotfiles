#!/usr/bin/env bash

### GENERIC

say() {
  # print command - %q escapes special shell characters
  {
    printf '$' && printf ' %q' "$@" && printf '\n'
  } >&2
  "$@"
}

try() {
  # print command - %q escapes special shell characters
  {
    printf '$' && printf ' %q' "$@" && printf '\n'
  } >&2 || exit
  "$@" || exit
}

### FUNCTIONS
# TODO Run this in a separate process (use the self-invoke trick).

preserve() {
  cd "$1" || { 
    RET=$?
    echo; echo "!!! ERR: cannot preserve $1 !!!"; echo
    return $RET 
  }
  try git add --all
  # Check if anything was staged (added to the index)
  if ! say git diff-index --quiet HEAD
  then
    try git commit --message="$(date +'%A %d %B (%d/%m/%Y)')"
    try git push
  fi
}

### START

# preserve dotfiles last as they contain the self-preservation script
preserve ~/dotfiles/
