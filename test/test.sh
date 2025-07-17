#!/bin/sh
echo =========================
echo testing with $(which awk)
echo
PATH="$PATH:$(dirname $(dirname $0))/src"
bad=0
for arch in riscv; do
    for f in $(find $(dirname $0)/*  -maxdepth 0 -type d -not -name awks); do
        echo $f
        cc.awk $f/test.c -a $arch > $f/test.s || { echo compilation failed; bad=1; continue; }
        riscv64-unknown-linux-musl-gcc $f/harness.c $f/test.s -o $f/a.out || { echo assemblation failed; bad=1; continue; }
        $f/a.out && echo success || { echo failed; bad=1; }
        echo
    done
done
exit $bad
