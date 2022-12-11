#!/usr/bin/env python3

from sys import argv
import solution


if __name__ == "__main__":
    if len(argv) == 2:
        f = solution.get(argv[1])
        if f is None:
            print ("No solution", argv[1], "registered")
            exit(1)
        f()
    else:
            print (f"Usage: python3 {argv[0]} [problem_name]")
            exit (2)