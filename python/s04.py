#!/usr/bin/env python3
from sys import stdin


def fully_contains(i1, i2):
    """Checks whether i1 is fully contained in i2 or vice versa."""
    x1, y1 = i1
    x2, y2 = i2
    assert x1 <= y1 and x2 <= y2
    return (x1 <= x2 and y2 <= y1) or (x2 <= x1 and y1 <= y2)


def overlaps(i1, i2):
    """"Checks whether i1 and i2 overlaps """
    x1, y1 = i1
    x2, y2 = i2
    return x1 <= x2 <= y1 or x2 <= x1 <= y2
    # x1------------y1
    # x2------------y2


def solve(do_part1):
    total = 0
    for line in stdin:
        fields = line.strip().split(',')
        i1 = [int(x) for x in fields[0].split("-")]
        i2 = [int(x) for x in fields[1].split("-")]
        if do_part1:
            if fully_contains(i1, i2):
                total += 1
        else:
            if overlaps(i1, i2):
                total += 1
    print(total)


def solve_part1():
    solve(True)


def solve_part2():
    solve(False)
