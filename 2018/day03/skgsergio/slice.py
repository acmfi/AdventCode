#!/usr/bin/env python3
import sys

import re
from collections import defaultdict


def solve(d):
    m = defaultdict(list)

    for r in re.finditer(r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)", d):
        c, l, t, w, h = map(int, r.groups())
        for i in range(l, l + w):
            for j in range(t, t + h):
                m[(i, j)].append(c)

    plus1 = [i for i in m if len(m[i]) > 1]
    part1 = len(plus1)

    part2 = None

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read()

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
