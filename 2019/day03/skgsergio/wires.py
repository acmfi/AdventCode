#!/usr/bin/env python3
import sys

from typing import List, Set, Tuple


M = {'L': (-1, 0), 'R': (1, 0), 'U': (0, 1), 'D': (0, -1)}


def points(data: List[str]) -> Set[Tuple[int, int]]:
    x, y = 0, 0
    p = set()

    for m in data:
        for _ in range(int(m[1:])):
            x += M[m[0]][0]
            y += M[m[0]][1]

            p.add((x, y))

    return p


def part1(path1: List[str], path2: List[str]) -> int:
    p1 = points(path1)
    p2 = points(path2)
    p = set(p1) & set(p2)

    return min([abs(x) + abs(y) for (x, y) in p])


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.readlines()

    path1 = d[0].split(',')
    path2 = d[1].split(',')

    print(f"Part 1: {part1(path1, path2)}")
