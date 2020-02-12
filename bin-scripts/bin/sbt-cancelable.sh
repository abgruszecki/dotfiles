#!/usr/bin/env bash

exec sbt --no-colors ";set cancelable in Global := true ;shell"
