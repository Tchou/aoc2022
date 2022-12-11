#!/usr/bin/env python3
from sys import stdin

def top_k(tab, v):
    """if v is larger than the smallest value of tab,
       v replaces it, otherwise tab is unchanged"""
    min_idx = -1
    min_v = None
    for i in range(len(tab)):
        if min_v is None or tab[i] < min_v:
            min_v = tab[i]
            min_idx = i
    if min_v is not None and v > min_v:
        tab[min_idx] = v


def solve(n):
    tab = []
    total_cal = 0
    for line in stdin:
        l = line.strip()
        if len(l) == 0:
            if len(tab) < n:
                tab.append(total_cal)
            else:
                top_k(tab, total_cal)
            total_cal = 0
        else:
            total_cal += int(l)

    total = 0
    for v in tab:
        total += v
    print(total)

def solve_part1():
    solve(1)

def solve_part2():
    solve(3)