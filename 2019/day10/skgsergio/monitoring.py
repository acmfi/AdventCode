#!/usr/bin/env python3
import sys
import math
import cmath
import itertools

from typing import List, Tuple, Optional


def part1(data: List[str]) -> Tuple[int, Tuple[int, int]]:
    maxc = 0
    best = (-1, -1)

    for x in range(len(data)):
        for y in range(len(data[x])):
            if data[x][y] == '.':
                continue

            c = -1
            for i in range(len(data)):
                for j in range(len(data[i])):
                    if data[i][j] == '.':
                        continue

                    gcd = math.gcd(i - x, j - y)
                    blocked = False

                    for q in range(1, gcd):
                        if data[x + ((i - x) // gcd) * q][y + ((j - y) // gcd) * q] == '#':
                            blocked = True
                            break

                    if not blocked:
                        c += 1

            maxc = max(c, maxc)

            if c == maxc:
                best = (x, y)

    return maxc, best


def phase(a, b):
    return cmath.phase(complex(a[0] - b[0], a[1] - b[1]))


def hypot(a, b):
    return math.hypot(a[0] - b[0], a[1] - b[1])


def part2(data: List[str], bet: int, best: Tuple[int, int]) -> int:
    vaporized = (-1, -1)

    ast = [(x, y) for x in range(len(data)) for y in range(len(data[x])) if data[x][y] == '#']
    ast.sort(key=lambda p: phase(p, best), reverse=True)

    groups = [list(g) for _, g in itertools.groupby(ast, key=lambda p: phase(p, best))]

    i = 0
    for it in range(bet):
        ini = i
        while not groups[i]:
            i = (i + 1) % len(groups)
            if i == ini:
                raise Exception(f"Vaporization {it}: No asteroids left.")

        vaporized = min(groups[i], key=lambda p: hypot(p, best))
        groups[i][:] = [p for p in groups[i] if p != vaporized]

        i = (i + 1) % len(groups)

    return vaporized[1] * 100 + vaporized[0] if vaporized else 0


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    p1, p1best = part1(d)
    print(f"Part 1: {p1}")
    print(f"Part 2: {part2(d, 200, p1best)}")
