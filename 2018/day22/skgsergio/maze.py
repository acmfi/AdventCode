#!/usr/bin/env python3
import sys

import heapq


class ErosionLevel:
    def __init__(self, depth, tx, ty):
        self._depth = depth
        self._erosion = {
            (0, 0): 0,
            (tx, ty): 0
        }

    def __getitem__(self, key):
        if key not in self:
            x, y = key

            if y == 0:
                gidx = x * 16807

            elif x == 0:
                gidx = y * 48271

            else:
                gidx = self[(x - 1, y)] * self[(x, y - 1)]

            self[key] = (gidx + self._depth) % 20183

        return self._erosion[key]

    def __setitem__(self, key, value):
        self._erosion[key] = value

    def __contains__(self, key):
        return key in self._erosion

    def __repr__(self):
        return f"{self._depth}; {self._erosion}"


def solve(d):
    depth = int(d[0].split(':')[1].strip())
    tx, ty = map(int, d[1].split(':')[1].strip().split(','))

    erosion = ErosionLevel(depth, tx, ty)

    # Part 1
    part1 = sum(erosion[x, y] % 3 for x in range(tx + 1) for y in range(ty + 1))

    # Part 2
    target = (tx, ty, 1)
    queue = [(0, 0, 0, 1)]  # mins is the first element for heapq to use it as priority
    best = {}

    part2 = None
    while queue:
        mins, x, y, cant = heapq.heappop(queue)
        k = (x, y, cant)

        if k in best and best[k] <= mins:
            continue

        best[k] = mins

        if k == target:
            part2 = best[k]
            break

        for c in range(3):
            if c != cant and c != erosion[x, y] % 3:
                heapq.heappush(queue, (mins + 7, x, y, c))

        for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nx = x + dx
            ny = y + dy

            if nx < 0 or ny < 0 or erosion[nx, ny] % 3 == cant:
                continue

            heapq.heappush(queue, (mins + 1, nx, ny, cant))

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
