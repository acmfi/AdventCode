#!/usr/bin/env python3
import sys

from collections import defaultdict


def findpwr(part, grid, ss=None):
    mpwr = float('-inf')
    mx = 0
    my = 0
    ms = 0

    if ss:
        ss_min = ss
        ss_max = ss + 1
    else:
        ss_min = 1
        ss_max = grid + 1

    for s in range(ss_min, ss_max):
        for x in range(s, grid):
            for y in range(s, grid):
                pwr = part[x, y] - part[x, y - s] - part[x - s, y] + part[x - s, y - s]

                if pwr > mpwr:
                    mpwr = pwr
                    mx = x - s + 1
                    my = y - s + 1
                    ms = s

    return mx, my, ms


def solve(gsn, grid=300):
    part = defaultdict(int)

    for x in range(grid):
        for y in range(grid):
            part[x, y] = (int(((x + 10) * y + gsn) * (x + 10) / 100) % 10) - 5
            part[x, y] += part[x, y - 1] + part[x - 1, y] - part[x - 1, y - 1]

    part1 = ','.join(map(str, findpwr(part, grid, 3)[:2]))

    part2 = ','.join(map(str, findpwr(part, grid)))

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>\n\tMy input: 6303",
              file=sys.stderr)
        exit(1)

    part1, part2 = solve(int(sys.argv[1]))

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
