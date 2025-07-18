#!/usr/bin/env python
import argparse
import subprocess
from pathlib import Path

dir = Path(__file__).parent
cc = Path(dir.parent, "src", "cc.awk")


def main():
    awks = [p for p in Path(dir, "awks").glob("*")]
    arches = ["riscv"]
    tests = [p for p in dir.iterdir() if p.is_dir() and p.name != "awks"]

    parser = argparse.ArgumentParser()
    parser.add_argument("--awk", choices=[awk.name for awk in awks])
    parser.add_argument("--arch", choices=arches)
    args = parser.parse_args()

    if args.awk:
        awks = [awk for awk in awks if awk.name == args.awk]
    else:
        awks = [awk for awk in awks if awk.name != "gawk"]

    gcc_for_arch = {"riscv": "riscv64-unknown-linux-musl-gcc"}

    total = 0
    bad = 0

    for arch in arches:
        for awk in awks:
            print(f"=== {arch}/{awk.name} ===")

            for test in tests:
                total += 1
                print(test.name)
                try:
                    subprocess.run(
                        [cc, test / "test.c", "-a", "riscv", "-o", test / "test.s"],
                        check=True,
                    )
                    subprocess.run(
                        [
                            gcc_for_arch[arch],
                            test / "test.s",
                            test / "harness.c",
                            "-o",
                            test / "a.out",
                        ],
                        check=True,
                    )
                    print("good")
                except Exception:
                    print("FAILED")
                    bad += 1

            print("\n")

    exit(bad)


if __name__ == "__main__":
    main()
