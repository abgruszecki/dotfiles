#!/usr/bin/env bash

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

### START

try cd ~/dotfiles
try git add --all
try git commit --message="$(date +'%A %d %B (%d/%m/%Y)')"
try git push
