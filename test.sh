#!/bin/sh
for f in $(find test/*  -maxdepth 1 -type d); do
    echo $f

    if ./c.awk $f/test.c > $f/test.s; then
        true
    else
        echo compilation failed
        continue
    fi
    riscv64-unknown-linux-musl-gcc $f/harness.c $f/test.s -o $f/a.out
    if $f/a.out; then
        echo success
    else
        echo failed
    fi
done
