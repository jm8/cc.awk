# cc.awk

A C compiler written in Awk.

It targets riscv assembly, so you will need to install qemu and riscv64-unknown-linux-musl-gcc (for the assembler)

```bash
cc.awk program.c > program.asm
riscv64-unknown-linux-musl-gcc program.asm -o program
./program 
```
