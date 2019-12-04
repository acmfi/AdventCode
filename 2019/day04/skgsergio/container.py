#!/usr/bin/env python3
import sys

from collections import Counter


def part1(init: int, end: int) -> int:
    c = 0

    for n in map(str, range(init, end + 1)):
        if list(n) == sorted(n) and max(Counter(n).values()) >= 2:
            c += 1

    return c


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <356261-846303>", file=sys.stderr)
        exit(1)

    init, end = map(int, sys.argv[1].split("-"))

    print(f"Part 1: {part1(init, end)}")
