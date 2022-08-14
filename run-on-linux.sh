#!/usr/bin/env bash
set -eu

# $system  "x86_64-linux"
if [ "$system" = "x86_64-linux" ]; then
    $@
else 
    docker run --rm --platform linux/x86_64 -v $PWD:/workdir -w /workdir alpine:3 $@
fi
