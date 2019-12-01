#!/usr/bin/env python3
import sys


def sum_plants(pots, orig_len):
    diff = (len(pots) - orig_len) // 2
    return sum((i - diff) for i, c in enumerate(pots) if c == '#')


def grow(pots, rules, gens):
    olen = len(pots)
    diff = 0
    stable_gen = None

    for gen in range(gens):
        pots = f"....{pots}...."
        growth = ""

        for x in range(2, len(pots) - 2):
            growth += rules[pots[x-2:x+3]]

        if growth in pots:
            stable_gen = gen
            diff = sum_plants(growth, olen) - sum_plants(pots, olen)
            break

        pots = growth

    if stable_gen and gens > stable_gen:
        return (gens - stable_gen) * diff + sum_plants(pots, olen)

    else:
        return sum_plants(pots, olen)


def solve(d):
    init = d[0].split(': ')[1]
    rules = {}

    for r in d[2:]:
        rs = r.split(' => ')
        rules[rs[0]] = rs[1]

    part1 = grow(init, rules, 20)

    part2 = grow(init, rules, 50000000000)

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
