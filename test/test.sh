#!/bin/sh
PATH="$PATH:$(dirname $(dirname $0))/src"
bad=0
for f in $(find $(dirname $0)/*  -maxdepth 1 -type d); do
    echo
    echo $f

    cc.awk $f/test.c > $f/test.s || { echo compilation failed; bad=1; continue; }
    riscv64-unknown-linux-musl-gcc $f/harness.c $f/test.s -o $f/a.out || { echo assemblation failed; bad=1; continue; }
    $f/a.out && echo success || { echo failed; bad=1; }
done

exit $bad
