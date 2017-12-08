#!/usr/bin/env python3

import sys

from collections import defaultdict


def part_1(instructions):
    ops = {
        '>': (lambda x, y: x > y),
        '<': (lambda x, y: x < y),
        '>=': (lambda x, y: x >= y),
        '<=': (lambda x, y: x <= y),
        '==': (lambda x, y: x == y),
        '!=': (lambda x, y: x != y),
        'inc': (lambda x, y: x + y),
        'dec': (lambda x, y: x - y),
    }

    v = defaultdict(int)

    for i in instructions:
        if ops[i[5]](v[i[4]], int(i[6])):
            v[i[0]] = ops[i[1]](v[i[0]], int(i[2]))

    return v[max(v.keys(), key=(lambda k: v[k]))]


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    ins = []
    with open(sys.argv[1], 'r') as f:
        for l in f:
            i = l.split()
            if len(i) != 0:
                ins.append(i)

    print("Part 1: {}".format(part_1(ins)))
