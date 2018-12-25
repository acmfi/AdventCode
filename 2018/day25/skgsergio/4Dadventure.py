#!/usr/bin/env python3
import sys

from collections import defaultdict, deque


def solve(d):
    p = []
    for l in d:
        p.append(tuple(map(int, l.split(','))))

    constellations = defaultdict(set)
    for x, (xx, xy, xz, xw) in enumerate(p):
        for y, (yx, yy, yz, yw) in enumerate(p):
            if abs(xx - yx) + abs(xy - yy) + abs(xz - yz) + abs(xw - yw) <= 3:
                constellations[x].add(y)

    part1 = 0
    seen = set()
    for idx in range(len(p)):
        if idx not in seen:
            part1 += 1

            v = deque([idx])
            while v:
                x = v.popleft()

                if x not in seen:
                    seen.add(x)

                    for y in constellations[x]:
                        v.append(y)

    part2 = 'underflow'

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
