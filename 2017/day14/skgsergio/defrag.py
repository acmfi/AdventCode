#!/usr/bin/env python3

import sys

sys.path.append('../../day10/skgsergio')
knot = __import__('knot', globals(), locals(), [], 0)


def solve(k):
    u = []
    for i in range(128):
        h = knot.part_2(f'{k}-{i}')
        bh = bin(int(h, 16))[2:].zfill(128)
        u += [(i, j) for j, d in enumerate(bh) if d == '1']

    lu = len(u)

    c = 0
    while u:
        q = [u[0]]

        while q:
            x, y = q.pop()

            if (x, y) in u:
                q += [(x - 1, y),
                      (x + 1, y),
                      (x, y + 1),
                      (x, y - 1)]
                u.remove((x, y))

        c += 1

    return lu, c


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input_str>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    part_1, part_2 = solve(sys.argv[1])
    print("Part 1: {}".format(part_1))
    print("Part 2: {}".format(part_2))
