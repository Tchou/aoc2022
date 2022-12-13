#!/usr/bin/env python3
from sys import stdin

def solve(n):
    cache = [-n] * 26 #stores position in the input stream were this char was read
    i = 0
    count = 0
    while True:
        if count == n:
            return i
        c = stdin.read(1)
        if c == '' or c == '\n':
            return -1 #end of file reached
        c_idx = ord(c) - ord('a')
        if i - cache[c_idx] > count:
            #this character was read for the last time more than n char ago
            #it is now the only occurrence in the last n chars
            count += 1
        else:
            #this character was read for the last time len_distinct chars ago
            count = i - cache[c_idx]
        cache[c_idx] = i

        i += 1

def solve_part1():
    print(solve(4))

def solve_part2():
    print(solve(14))