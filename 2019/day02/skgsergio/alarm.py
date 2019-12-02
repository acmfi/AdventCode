#!/usr/bin/env python3
import sys

from typing import List


def program(data: List[int], n: int, v: int) -> int:
    p = data[:]
    p[1] = n
    p[2] = v
    pc = 0

    while p[pc] != 99:
        if p[pc] == 1:
            p[p[pc + 3]] = p[p[pc + 1]] + p[p[pc + 2]]

        elif p[pc] == 2:
            p[p[pc + 3]] = p[p[pc + 1]] * p[p[pc + 2]]

        pc += 4

    return p[0]


def brute(data: List[int], target: int) -> int:
    for n in range(100):
        for v in range(100):
            if program(data, n, v) == target:
                return 100 * n + v

    return 0


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        d = f.read().split(',')

    d = list(map(int, d))

    print(f"Part 1: {program(d, 12, 2)}")
    print(f"Part 2: {brute(d, 19690720)}")
