#!/usr/bin/env python3

import sys
from itertools import count
from collections import defaultdict


def sum_n(d, i, j):
    l = [d[x, y] for x in range(i - 1, i + 2) for y in range(j - 1, j + 2)]
    return sum(l)


def spiral_generator():
    spiral = defaultdict(int)
    spiral[0, 0] = 1

    i = 0
    j = 0
    for n in count(1, 2):
        for _ in range(n):
            i += 1
            spiral[i, j] = sum_n(spiral, i, j)
            yield spiral[i, j]

        for _ in range(n):
            j -= 1
            spiral[i, j] = sum_n(spiral, i, j)
            yield spiral[i, j]

        for _ in range(n + 1):
            i -= 1
            spiral[i, j] = sum_n(spiral, i, j)
            yield spiral[i, j]

        for _ in range(n + 1):
            j += 1
            spiral[i, j] = sum_n(spiral, i, j)
            yield spiral[i, j]


def part_2(x):
    for n in spiral_generator():
        if n > x:
            return n


def part_1(x):
    l = 1

    while (l ** 2) < x:
        l += 2

    return int((l - 1) / 2) + int((l / 2) - (((l ** 2) - x) % (l - 1)))


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: {} <number>".format(sys.argv[0]), file=sys.stderr)

    else:
        n_in = int(sys.argv[1])

        print("Part 1: {}".format(part_1(n_in)))
        print("Part 2: {}".format(part_2(n_in)))
