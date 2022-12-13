#!/usr/bin/env python3

from sys import argv
import solution


if __name__ == "__main__":
    if len(argv) == 2:
        arg = argv[1]
        if arg == "list":
            print("Available solutions:")
            for l in solution.list():
                print("    ", l)

            exit(0)

        f = solution.get(arg)
        if f is None:
            print("Error: unknown solution", arg)
            exit(1)
        f()
    else:
        print(f"Usage: python3 {argv[0]} [problem_name]")
        exit(2)
