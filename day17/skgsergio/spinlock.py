#!/usr/bin/env python3

import sys


def part_1(n):
    s = [0]
    p = 0

    for i in range(2017):
        p = ((p + n) % len(s)) + 1
        s.insert(p, i + 1)

    return s[p + 1]


def part_2(n):
    p = 0
    s = 0

    for i in range(50000000):
        p = ((p + n) % (i + 1)) + 1

        if p == 1:
            s = i + 1

    return s


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    n = int(sys.argv[1])

    print("Part 1: {}".format(part_1(n)))
    print("Part 2: {}".format(part_2(n)))
