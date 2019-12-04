#!/usr/bin/env python3
import sys

from collections import Counter

from typing import Tuple


def solve(init: int, end: int) -> Tuple[int, int]:
    p1c = 0
    p2c = 0

    for n in map(str, range(init, end + 1)):
        if list(n) == sorted(n):
            c = Counter(n).values()
            p1c += max(c) >= 2
            p2c += 2 in c

    return p1c, p2c


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <356261-846303>", file=sys.stderr)
        exit(1)

    init, end = map(int, sys.argv[1].split("-"))

    part1, part2 = solve(init, end)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
