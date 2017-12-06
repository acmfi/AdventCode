#!/usr/bin/env python3

import sys


def part_1(l):
    p = []

    while l not in p:
        p.append(l[:])

        m = max(l)

        i = l.index(m)
        l[i] = 0

        while m != 0:
            i = (i + 1) % len(l)
            l[i] += 1
            m -= 1

    return len(p)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    l = None
    with open(sys.argv[1], 'r') as f:
        l = list(map(int, f.read().split()))

    print(part_1(l))
