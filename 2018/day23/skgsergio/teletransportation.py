#!/usr/bin/env python3
import sys

import re
from collections import namedtuple


NANO_RE = re.compile(r"pos=<(-?[0-9]+),(-?[0-9]+),(-?[0-9]+)>,\s+r=([0-9]+)")


class Nanobot(namedtuple('Nanobot', ['x', 'y', 'z', 'radius'])):
    def dist(self, other):
        return abs(self.x - other.x) + abs(self.y - other.y) + abs(self.z - other.z)


def solve(d):
    bots = []

    for cur in d:
        m = NANO_RE.match(cur)
        x, y, z, radius = map(int, m.groups())
        bots.append(Nanobot(x, y, z, radius))

    # Part 1
    mxrb = max(bots, key=lambda b: b.radius)
    part1 = sum(mxrb.dist(b) <= mxrb.radius for b in bots)

    # Part 2
    mxx = max(bots, key=lambda b: b.x).x
    mnx = min(bots, key=lambda b: b.x).x
    mxy = max(bots, key=lambda b: b.y).y
    mny = min(bots, key=lambda b: b.y).y
    mxz = max(bots, key=lambda b: b.z).z
    mnz = min(bots, key=lambda b: b.z).z

    div = 1
    while div < mxx - mnx:
        div *= 2

    part2 = None
    while not part2:
        bc = (0, 0, 0, 0)

        for z in range(mnz, mxz + 1, div):
            for y in range(mny, mxy + 1, div):
                for x in range(mnx, mxx + 1, div):
                    pt = Nanobot(x, y, z, None)
                    c = sum((b.dist(pt) - b.radius) // div <= 0 for b in bots)

                    if c < bc[-1] or \
                       (c == bc[-1] and (abs(x) + abs(y) + abs(z)) > (abs(bc[0]) + abs(bc[1]) + abs(bc[2]))):
                        continue

                    bc = (x, y, z, c)

        if div == 1:
            part2 = abs(bc[0]) + abs(bc[1]) + abs(bc[2])

        else:
            mxx, mnx = (bc[0] + div), (bc[0] - div)
            mxy, mny = (bc[1] + div), (bc[1] - div)
            mxz, mnz = (bc[2] + div), (bc[2] - div)
            div //= 2

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
