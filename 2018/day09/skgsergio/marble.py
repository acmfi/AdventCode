#!/usr/bin/env python3
import sys

from collections import defaultdict, deque


def solve(d):
    pls = int(d.split()[0])
    pts = int(d.split()[6])

    s = defaultdict(int)
    c = deque([0])

    for m in range(1, pts + 1):
        if (m % 23) == 0:
            c.rotate(7)
            s[m % pls] += (m + c.pop())
            c.rotate(-1)
        else:
            c.rotate(-1)
            c.append(m)

    part1 = max(s.values())

    part2 = None

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    for l in d:
        print(l)
        part1, part2 = solve(l)

        print(f"Part 1: {part1}")
        print(f"Part 2: {part2}")
