#!/usr/bin/env python3
import sys

from collections import defaultdict, deque


def marble(pls, pts):
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

    return max(s.values())


def solve(d):
    pls = int(d.split()[0])
    pts = int(d.split()[6])

    part1 = marble(pls, pts)

    part2 = marble(pls, (pts * 100))

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read().strip()

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
