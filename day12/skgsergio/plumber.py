#!/usr/bin/env python3

import sys

from collections import defaultdict


def solve(gl):
    g = defaultdict(list)

    for l in gl:
        e = l.replace(',', '').split()

        x = int(e[0])
        ys = map(int, e[2:])

        for y in ys:
            g[x].append(y)
            g[y].append(x)

    v = []
    c = 0
    l = None
    for i in range(len(gl)):
        if i in v:
            continue

        c += 1

        s = [i]
        while len(s) > 0:
            x = s.pop()

            for y in g[x]:
                if y not in v:
                    v.append(y)
                    s.append(y)

        if not l:
            l = len(v)

    return l, c


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    gl = None
    with open(sys.argv[1], 'r') as f:
        gl = [l.rstrip('\n') for l in f]

    part_1, part_2 = solve(gl)
    print("Part 1: {}".format(part_1))
    print("Part 2: {}".format(part_2))
