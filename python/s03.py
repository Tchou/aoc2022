#!/usr/bin/env python3
from sys import stdin

def priority(s):
    assert len(s) == 1 and ('A' <= s <= 'Z' or 'a' <= s <= 'z')
    if 'A' <= s <= 'Z':
        return ord(s) - ord('A') + 27
    else:
        return ord(s) - ord('a') + 1

def find_common_item(s):
    dict = {}
    for i in range(len(s)//2):
        dict[s[i]] = 1
    for i in range(len(s)//2, len(s)):
        if s[i] in dict:
            return priority(s[i])
    #should not happen
    return 0


def solve_part1 ():
    total = 0
    for line in stdin:
        total += find_common_item(line.strip())

    print(total)


def find_common_badge(tab):
    prev_dict = None
    for line in tab:
        next_dict = {}
        for c in line:
            if prev_dict is None or c in prev_dict:
                next_dict[c] = 1
        prev_dict = next_dict

    assert len(prev_dict) == 1 #Problem states that there is only one badge in common
    for c in prev_dict.keys():
        return priority(c)


def solve_part1_alt ():
    total = 0
    for line in stdin:
        s = line.strip()
        total += find_common_badge([s[0:len(s)//2], s[len(s)//2:len(s)]])

    print(total)
def solve_part2 ():
    total = 0
    while True:
        try:
            l1 = input()
            l2 = input()
            l3 = input()
            total += find_common_badge([l1, l2, l3])
        except EOFError:
            break
    print(total)


