#!/usr/bin/env python3
import sys

import re
from collections import defaultdict


def solve(d):
    m = defaultdict(list)
    o = {}

    for r in re.finditer(r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)", d):
        c, l, t, w, h = map(int, r.groups())
        o[c] = set()
        for i in range(l, l + w):
            for j in range(t, t + h):
                if len(m[(i, j)]) > 0:
                    for n in m[(i, j)]:
                        o[n].add(c)
                        o[c].add(n)
                m[(i, j)].append(c)

    plus1 = [i for i in m if len(m[i]) > 1]
    part1 = len(plus1)

    notov = [c for c in o if len(o[c]) == 0]
    part2 = notov[0]

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read()

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
