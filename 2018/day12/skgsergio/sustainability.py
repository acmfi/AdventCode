#!/usr/bin/env python3
import sys


def sum_plants(pots):
    diff = (len(pots) - 100) // 2
    return sum((i - diff) for i, c in enumerate(pots) if c == '#')


def solve(d):
    init = d[0].split(': ')[1]
    rules = {}

    for r in d[2:]:
        rs = r.split(' => ')
        rules[rs[0]] = rs[1]

    pots = init

    for i in range(20):
        pots = f"....{pots}...."
        growth = ""

        for x in range(2, len(pots) - 2):
            growth += rules[pots[x-2:x+3]]

        pots = growth

    part1 = str(sum_plants(pots))

    part2 = None

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
