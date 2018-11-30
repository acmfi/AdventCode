#!/usr/bin/env python3

import sys


def solve(dirs):
    x = 0
    y = 0
    z = 0

    dst = []

    for d in dirs:
        if d == "n":
            y += 1
            z -= 1
        elif d == "ne":
            z -= 1
            x += 1
        elif d == "se":
            x += 1
            y -= 1
        elif d == "s":
            y -= 1
            z += 1
        elif d == "sw":
            z += 1
            x -= 1
        elif d == "nw":
            x -= 1
            y += 1

        dst.append(int((abs(x) + abs(y) + abs(z)) / 2))

    return dst[-1], max(dst)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    dirs = None
    with open(sys.argv[1], 'r') as f:
        dirs = f.read().strip().split(',')

    part_1, part_2 = solve(dirs)
    print("Part 1: {}".format(part_1))
    print("Part 2: {}".format(part_2))
