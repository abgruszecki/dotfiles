#!/usr/bin/env sh

function maybesource {
    test -f "$1" && . "$1"
}


prefix=~/.bash.d

. $prefix/universal.sh
maybesource $prefix/local/env.sh
maybesource $prefix/local/functions.sh
