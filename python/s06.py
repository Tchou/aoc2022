#!/usr/bin/env python3
from sys import stdin


# we use an array of fixed size as a ring buffer.


def ring_buffer(capacity):
    """Creates a new ring buffer with the given capacity."""
    return {"data": [None] * capacity, "first": 0, "length": 0}


def capacity(rb):
    """Returns the capacity of the ring buffer."""
    return len(rb["data"])


def length(rb):
    """Returns the number of elements in the ring buffer."""
    return rb["length"]


def put(rb, e):
    """Puts element e at the end of the ring buffer. Fails if
       rb is at full capacity."""
    length = rb["length"]
    cap = capacity(rb)
    assert length < cap
    first = rb["first"]
    last = (first + length) % cap  # wrap around
    rb["data"][last] = e
    rb["length"] += 1


def get(rb):
    """Returns the first element of rb. Fails if rb is empty"""
    assert rb["length"] > 0
    e = rb["data"][rb["first"]]
    rb["first"] = (rb["first"] + 1) % capacity(rb)
    return e


def put_unique(rb, e):
    """Puts element e at the end of rb. If another occurence of element e exists in
       rb, all elements from the front to the first occurence included are discarded.
       Equality between elements is tested with ==.
       """
    first = rb["first"]
    cap = capacity(rb)
    for i in range(rb["length"]):
        idx = (first + i) % cap
        o = rb["data"][idx]
        if o == e:
            rb["first"] = (idx + 1) % cap
            rb["length"] -= i + 1
            break
    put(rb, e)

def solve(n):
    line = input()
    rb = ring_buffer(n)
    for i in range(len(line)):
        c = line[i]
        put_unique(rb, c)
        if length(rb) == n:
            return i+1 #Characters are 1-based in the problem


    return -1 #we never found a subsequence of n distinct characters.

def solve_part1():
    print(solve(4))

def solve_part2():
    print(solve(14))