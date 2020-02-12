#!/usr/bin/env bash

if ! [[ -f .dotty-ide-artifact ]]
then
	echo >&2 "No .dotty-ide-artifact, is this the right dir?"
	exit 1
fi

coursier launch "$(cat .dotty-ide-artifact)" -M dotty.tools.languageserver.Main -- -stdio
