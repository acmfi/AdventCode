#!/usr/bin/env python3
import sys

from collections import defaultdict


def solve(d):
    cs = set(tuple(map(int, c.split(','))) for c in d)

    x_max = max(cs, key=lambda x: x[0])[0]
    y_max = max(cs, key=lambda x: x[1])[1]

    cs_rg = dict(enumerate(cs, start=1))
    sz = defaultdict(int)
    inf = set()

    limit = 10000
    shared = 0

    for x in range(x_max + 1):
        for y in range(y_max + 1):
            m_dist = sorted([(abs(px - x) + abs(py - y), rg) for rg, (px, py) in cs_rg.items()])
            shared += (sum(d[0] for d in m_dist) < limit)

            if len(m_dist) == 1 or m_dist[0][0] != m_dist[1][0]:
                rg = m_dist[0][1]
                sz[rg] += 1

                if x == 0 or x == x_max or y == 0 or y == y_max:
                    inf.add(rg)

    part1 = max(s for rg, s in sz.items() if rg not in inf)

    part2 = shared

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
