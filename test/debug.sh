#!/bin/sh
cd $(dirname $(dirname $0)) || exit 1
rlwrap gawk --debug -f src/cc.awk "$1"
