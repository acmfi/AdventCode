#!/usr/bin/env python3
import sys
from itertools import accumulate, cycle


def solve(i):
    c = [int(n.strip()) for n in i]

    part1 = sum(c)

    r = set([0])
    part2 = next(n for n in accumulate(cycle(c)) if n in r or r.add(n))

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    i = None
    with open(sys.argv[1], 'r') as f:
        i = f.readlines()

    part1, part2 = solve(i)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
