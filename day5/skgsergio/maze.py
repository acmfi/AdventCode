#!/usr/bin/env python3

import sys


def part_1(l):
    idx = 0
    steps = 0

    while 0 <= idx < len(l):
        n = l[idx]

        l[idx] = n + 1

        idx += n
        steps += 1

    return steps


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    l = None
    with open(sys.argv[1], 'r') as f:
        l = list(map(int, f.read().split()))

    print(part_1(l))
