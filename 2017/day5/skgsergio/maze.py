#!/usr/bin/env python3

import sys


def solve(l, part_2=False):
    idx = 0
    steps = 0

    while 0 <= idx < len(l):
        n = l[idx]

        l[idx] = n - 1 if part_2 and n >= 3 else n + 1

        idx += n
        steps += 1

    return steps


def part_1(l):
    return solve(l)


def part_2(jumps):
    return solve(l, True)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    l = None
    with open(sys.argv[1], 'r') as f:
        l = list(map(int, f.read().split()))

    print(part_1(l[:]))
    print(part_2(l[:]))
