#!/usr/bin/env python3
import sys


def tree(d):
    ch, me = d[:2]
    d = d[2:]

    tot = 0
    vs = []

    for _ in range(ch):
        t, v, d = tree(d)
        tot += t
        vs.append(v)

    tot += sum(d[:me])

    val = sum(vs[i - 1] for i in d[:me] if i > 0 and i <= len(vs)) if ch != 0 else sum(d[:me])

    return tot, val, d[me:]


def solve(d):
    lic = list(map(int, d.split()))

    part1, part2, _ = tree(lic)

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
