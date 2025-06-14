run:
    ./c.awk test.c > out.s
    riscv64-unknown-linux-musl-gcc out.s -o a.out
    bash -c './a.out; echo Exited with $?'