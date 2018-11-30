#!/usr/bin/env python3

import sys


def gen(n, x, m=1):
    while True:
        n = n * x % 2147483647
        if n % m == 0:
            yield n & 0xFFFF


def solve(a_n, b_n, rnds, m_a=1, m_b=1):
    a = gen(a_n, 16807, m_a)
    b = gen(b_n, 48271, m_b)
    c = 0

    for _ in range(rnds):
        if next(a) == next(b):
            c += 1

    return c


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input_str>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    a = int(sys.argv[1])
    b = int(sys.argv[2])
    print("Part 1: {}".format(solve(a, b, 40000000)))
    print("Part 2: {}".format(solve(a, b, 5000000, 4, 8)))
