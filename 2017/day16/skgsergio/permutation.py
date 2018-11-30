#!/usr/bin/env python3

import sys

from collections import deque


def solve(progs, ins, rnds=1):
    p = deque(progs)
    s = []

    for i in range(rnds):
        l = ''.join(p)

        if l in s:
            return s[rnds % i]

        s.append(l)

        for m in ins:
            if m[0] == 's':
                p.rotate(int(m[1:]))

            elif m[0] == 'x':
                a, b = map(int, m[1:].split('/'))
                p[a], p[b] = p[b], p[a]

            elif m[0] == 'p':
                a, b = m[1:].split('/')
                pa = p.index(a)
                pb = p.index(b)

                p[pa], p[pb] = p[pb], p[pa]

    return ''.join(p)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    ins = None
    with open(sys.argv[1], 'r') as f:
        ins = f.read().strip().split(",")

    progs = "abcdefghijklmnop"
    print("Part 1: {}".format(solve(progs, ins)))
    print("Part 2: {}".format(solve(progs, ins, 1000000000)))
