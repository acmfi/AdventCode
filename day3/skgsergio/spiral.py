#!/usr/bin/env python3

import sys


def part_1(x):
    l = 1

    while (l ** 2) < x:
        l += 2

    return int((l - 1) / 2) + int((l / 2) - (((l ** 2) - x) % (l - 1)))


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: {} <number>".format(sys.argv[0]), file=sys.stderr)

    else:
        print(part_1(int(sys.argv[1])))
