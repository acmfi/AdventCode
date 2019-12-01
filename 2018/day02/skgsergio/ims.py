#!/usr/bin/env python3
import sys


def solve(l):
    tw = 0
    th = 0

    for w in l:
        cs = set(w)
        tw += any(w.count(c) == 2 for c in cs)
        th += any(w.count(c) == 3 for c in cs)

    part1 = tw * th

    part2 = set(''.join([c1 for c1, c2 in zip(a, b) if c1 == c2]) for a in l[:-1] for b in l[1:] if sum([c1 != c2 for c1, c2 in zip(a, b)]) == 1)

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    i = None
    with open(sys.argv[1], 'r') as f:
        i = f.read().splitlines()

    part1, part2 = solve(i)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
