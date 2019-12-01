#!/usr/bin/env python3
import sys

from collections import defaultdict


DIRS = {
    'N': (0, -1),
    'E': (1, 0),
    'S': (0, 1),
    'W': (-1, 0)
}


def solve(d):
    pos = []
    dists = defaultdict(int)

    x, y = 5000, 5000

    for c in d[1:-1]:
        px, py = x, y

        if c == '(':
            pos.append((x, y))

        elif c == ')':
            x, y = pos.pop()

        elif c == '|':
            x, y = pos[-1]

        else:
            dx, dy = DIRS[c]
            x += dx
            y += dy

            if dists[(x, y)] != 0:
                dists[(x, y)] = min(dists[(x, y)], dists[(px, py)] + 1)

            else:
                dists[(x, y)] = dists[(px, py)] + 1

    part1 = max(dists.values())
    part2 = sum(x >= 1000 for x in dists.values())

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
