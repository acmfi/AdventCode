#!/usr/bin/env python3

import sys


def solve(l, rnds):
    h = list(range(256))

    n = 0
    s = 0

    for _ in range(rnds):
        for x in l:
            for i in range(x // 2):
                h1 = (n + i) % len(h)
                h2 = (n + x - 1 - i) % len(h)
                y = h[h1]
                h[h1] = h[h2]
                h[h2] = y
            n += x + s
            s += 1

    return h


def part_1(l):
    l2 = list(map(int, l.split(',')))

    h = solve(l2, 1)

    return h[0] * h[1]


def part_2(l):
    l2 = [ord(n) for n in l] + [17, 31, 73, 47, 23]
    h = solve(l2, 64)

    hexh = ''
    for i in range(len(h) // 16):
        n = 0
        for j in range(16):
            n ^= h[i * 16 + j]
        hexh += f'{n:02x}'

    return hexh


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    l = None
    with open(sys.argv[1], 'r') as f:
        l = f.readlines()[0].rstrip('\n')

    print("Part 1: {}".format(part_1(l)))
    print("Part 2: {}".format(part_2(l)))
