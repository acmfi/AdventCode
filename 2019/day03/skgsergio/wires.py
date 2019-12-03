#!/usr/bin/env python3
import sys

from typing import List, Dict, Tuple


M = {'L': (-1, 0), 'R': (1, 0), 'U': (0, 1), 'D': (0, -1)}


def points(data: List[str]) -> Dict[Tuple[int, int], int]:
    x, y = 0, 0
    c = 0
    p: Dict[Tuple[int, int], int] = {}

    for m in data:
        for _ in range(int(m[1:])):
            x += M[m[0]][0]
            y += M[m[0]][1]
            c += 1

            if (x, y) not in p:
                p[(x, y)] = c

    return p


def solve(path1: List[str], path2: List[str]) -> Tuple[int, int]:
    p1 = points(path1)
    p2 = points(path2)
    ps = set(p1.keys()) & set(p2.keys())

    dist = min([abs(x) + abs(y) for (x, y) in ps])
    steps = min([p1[p] + p2[p] for p in ps])

    return dist, steps


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.readlines()

    path1 = d[0].split(',')
    path2 = d[1].split(',')

    part1, part2 = solve(path1, path2)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
