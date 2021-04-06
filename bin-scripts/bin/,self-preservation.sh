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

preserve() {
  try cd "$1"
  try git add --all
  if ! say git diff-index --quiet HEAD
  then
    try git commit --message="$(date +'%A %d %B (%d/%m/%Y)')"
    try git push
  fi
}

### START

preserve ~/.emacs-profiles.d/spacemacs/private/bespoke-scala-mode/local/scala-mode
preserve ~/.emacs-profiles.d/spacemacs/private/

# preserve dotfiles last as they contain the self-preservation script
preserve ~/dotfiles/
