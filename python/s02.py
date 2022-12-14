from sys import stdin

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
    return ((4 - (op - my)) % 3)*3 + my + 1

def solve (offset, op):
    total = 0
    for line in stdin:
        fields = line.strip().split(" ")
        op_move = move(fields[0])
        my_move = move(fields[1])
        total += score(op_move, (my_move + offset + op*op_move) %3)
    print(total)

def solve_part1():
    solve(0,0)

def solve_part2():
    solve(2,1)

  