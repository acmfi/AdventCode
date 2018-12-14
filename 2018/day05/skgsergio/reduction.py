#!/usr/bin/env python3
import sys


def react(chain):
    poly = []

    for unit in chain:
        if poly and abs(ord(unit) - ord(poly[-1])) == 32:
            poly.pop()
        else:
            poly.append(unit)

    return poly


def solve(c):
    part1 = len(react(c))

    part2 = min([len(react(c.replace(u, '').replace(u.upper(), ''))) for u in set(c.lower())])

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
