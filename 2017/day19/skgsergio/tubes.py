#!/usr/bin/env python3

import sys


def solve(t):
    mv = {
        'D': lambda x, y: (x, y + 1),
        'U': lambda x, y: (x, y - 1),
        'R': lambda x, y: (x + 1, y),
        'L': lambda x, y: (x - 1, y)
    }

    x = t[0].index('|')
    y = 0

    d = 'D'
    c = None
    l = []
    s = 0

    while c != ' ':
        s += 1

        x, y = mv[d](x, y)

        c = t[y][x]
        if c == '+':
            if d in ('D', 'U'):
                d = 'R' if t[y][x - 1] == ' ' else 'L'
            else:
                d = 'D' if t[y - 1][x] == ' ' else 'U'

        elif c not in ('|', '-'):
            l.append(c)

    return ''.join(l), s


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    t = None
    with open(sys.argv[1], 'r') as f:
        t = f.readlines()

    part_1, part_2 = solve(t)

    print("Part 1: {}".format(part_1))
    print("Part 2: {}".format(part_2))
