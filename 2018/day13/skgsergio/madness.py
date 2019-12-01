#!/usr/bin/env python3
import sys

import re


CURVES = {
    '/': {
        '<': 'v',
        '>': '^',
        '^': '>',
        'v': '<'
    },
    '\\': {
        '<': '^',
        '>': 'v',
        '^': '<',
        'v': '>'
    }
}

INTDIRS = ('<', '|', '>')
INTRULES = {
    '<': {
        INTDIRS[0]: 'v',
        INTDIRS[1]: '<',
        INTDIRS[2]: '^',
    },
    '>': {
        INTDIRS[0]: '^',
        INTDIRS[1]: '>',
        INTDIRS[2]: 'v',
    },
    '^': {
        INTDIRS[0]: '<',
        INTDIRS[1]: '^',
        INTDIRS[2]: '>',
    },
    'v': {
        INTDIRS[0]: '>',
        INTDIRS[1]: 'v',
        INTDIRS[2]: '<',
    }
}

MOVEMENTS = {
    '<': lambda y, x: (y, x - 1),
    '>': lambda y, x: (y, x + 1),
    '^': lambda y, x: (y - 1, x),
    'v': lambda y, x: (y + 1, x),
}


def solve(d):
    track = []
    carts = []

    for y in range(len(d)):
        track.append(re.sub(r'[<>]', '-', re.sub(r'[\^v]', '|', d[y])))

        for c in re.finditer(r'[<>\^v]', d[y]):
            carts.append((y, c.start(), c.group(0), INTDIRS[0]))

    part1 = None
    part2 = None

    while not part2:
        crashed = set()

        for cn in range(len(carts)):
            (y, x, c, t) = carts[cn]

            if (y, x) in crashed:
                continue

            y, x = MOVEMENTS[c](y, x)

            if track[y][x] in CURVES.keys():
                c = CURVES[track[y][x]][c]

            elif track[y][x] == '+':
                c = INTRULES[c][t]
                t = INTDIRS[(INTDIRS.index(t) + 1) % len(INTDIRS)]

            if any(cy == y and cx == x for cy, cx, _, _ in carts):
                crashed.add((y, x))

                if not part1:
                    part1 = f"{x},{y}"

            carts[cn] = (y, x, c, t)

        carts = sorted([c for c in carts if (c[0], c[1]) not in crashed],
                       key=lambda c: (c[0], c[1]))

        if len(carts) == 1:
            part2 = f'{carts[0][1]},{carts[0][0]}'

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
