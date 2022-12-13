#!/usr/bin/env python3
from sys import stdin

# In the setup part of the input
# Each line is n * 4 chars where n is
# the number of stacks. Each stack is
# either: '    ' (4 spaces, nothing there)
# '[X] ' (object X there)
# this part finishes with the column indices
# 1    2 ... n which we can skip and
# an empty line which we can also skip
# the comes the list of moves.


def load_input():
    stacks = None
    while True:
        line = input()  # input does add \n
        if '[' in line:
            # still on a stack description line
            if stacks is None:
                # initialise n arrays that will be our stacks
                # don't use [[]] * n since otherwise the same
                # stack in memory is used n times !
                stacks = [[] for i in range((len(line) // 4) + 1)]
            for i in range(len(stacks)):
                idx = 1 + i * 4
                c = line[idx]
                if c != ' ':
                    stacks[i] += [c]
        else:
            # we just read the line with 1 2 3 ... n
            input()  # skip the empty line
            break
    for s in stacks:
        s.reverse()  # reverse the stacks in place
    moves = []
    try:
        while True:
            line = input()
            fields = line.split(" ")
            #[ "move", x, "from", y, "to", "z" ]
            move = (int(fields[1]), int(fields[3]), int(fields[5]))
            moves += [move]
    except EOFError:
        pass  # do nothing

    return (stacks, moves)


def print_stacks(stacks):
    # find the highest stack:
    hmax = 0
    for s in stacks:
        hmax = max(hmax, len(s))
    # print each stack starting from the end
    for level in range(hmax-1, -1, -1):
        for s in stacks:
            if level < len(s):
                print("[" + s[level] + "] ", end='')
            else:
                print("    ", end='')
        print()  # newline
    for i in range(len(stacks)):
        print(f"{1+i:2}  ", end='')
    print()


def do_moves9000 (stacks, moves):
    for (n, src, dst) in moves:
        for i in range (n):
            e = stacks[src-1].pop()
            stacks[dst-1] += [e]

def do_moves9001 (stacks, moves):
    for (n, src, dst) in moves:
        tmp = []
        for i in range (n):
            e = stacks[src-1].pop()
            tmp += [e]
        tmp.reverse()
        for e in tmp:
            stacks[dst-1] += [e]


def result(stacks):
    letters = [ s[len(s) - 1] for s in stacks ]
    return "".join(letters)

def solve_part1():
    stacks, moves = load_input()
    do_moves9000(stacks, moves)
    print_stacks(stacks)
    print (result(stacks))

def solve_part2():
    stacks, moves = load_input()
    do_moves9001(stacks, moves)
    print_stacks(stacks)
    print (result(stacks))
