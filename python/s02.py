from sys import stdin
import solution

## DEFINE 0 based constants instead of 1 based
## to have a simple modulo
ROCK = 0
PAPER = 1
SCISORS = 2

def move(s):
    if 'A' <= s <= 'C':
        return ord(s) - ord ('A')
    else:
        return ord(s) - ord ('X')


def score(op, my):
    res = 0
    if op == my:
        res = 3
    elif (op + 1) % 3 == my:
        #Victory
        res = 6
    return my + 1 + res


def solve_part1 ():
    total = 0
    for line in stdin:
        fields = line.strip().split(" ")
        op_move = move(fields[0])
        my_move = move(fields[1])
        total += score(op_move, my_move)
    print(total)


def move_for_result(op, res):
    if res == 'X':
        ## need to loose
        my = (op + 2) % 3
    elif res == 'Y':
        ## need to draw
        my = op
    else:
        ## need to win
        my = (op + 1) % 3
    return my

def solve_part2 ():
    total = 0
    for line in stdin:
        fields = line.strip().split(" ")
        op_move = move(fields[0])
        my_move = move_for_result(op_move,fields[1])
        total += score(op_move, my_move)
    print(total)


solution.register("02_part1", solve_part1)
solution.register("02_part2", solve_part2)
