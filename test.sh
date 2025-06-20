#!/bin/sh
for f in $(find test/*  -maxdepth 1 -type d); do
    echo
    echo $f

    ./cc.awk $f/test.c > $f/test.s || { echo compilation failed; continue; }
    riscv64-unknown-linux-musl-gcc $f/harness.c $f/test.s -o $f/a.out || { echo assemblation failed; continue; }
    $f/a.out && echo success || echo failed
done
