#!/usr/bin/env python3
import sys

import string
from collections import defaultdict


def solve(d):
    g = defaultdict(set)

    for l in d:
        f = l.split()
        g[f[7]].add(f[1])

    o = []
    a = True
    while a:
        a = False
        for j in string.ascii_uppercase:
            if j not in o and g[j] & set(o) == g[j]:
                o.append(j)
                a = True
                break

    part1 = ''.join(o)

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

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
