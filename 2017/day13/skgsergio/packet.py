#!/usr/bin/env python3

import sys


def part_1(r):
    s = 0

    for k, v in r.items():
        if k % (2 * v - 2) == 0:
            s += k * v

    return s


def part_2(r):
    ps = 0

    while True:
        for k, v in r.items():
            if (k + ps) % (2 * (v - 1)) == 0:
                break
        else:
            return ps
        ps += 1

    return ps


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    r = {}
    with open(sys.argv[1], 'r') as f:
        for l in f:
            l = l.strip().split(": ")
            r[int(l[0])] = int(l[1])

    print("Part 1: {}".format(part_1(r)))
    print("Part 2: {}".format(part_2(r)))
