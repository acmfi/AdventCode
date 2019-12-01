#!/usr/bin/env python3
import sys

from collections import defaultdict


def solve(d, spring=(500, 0)):
    clay = defaultdict(bool)

    for line in d:
        a, b = line.split(',')
        n = int(a.split('=')[1])
        mi, mf = map(int, b.split('=')[1].split('..'))

        if a[0] == 'x':
            for m in range(mi, mf + 1):
                clay[(n, m)] = True

        else:
            for m in range(mi, mf + 1):
                clay[(m, n)] = True

    ymin = min(clay, key=lambda p: p[1])[1]
    ymax = max(clay, key=lambda p: p[1])[1]

    flowing = set()
    still = set()

    falling = set([spring])
    filling = set()

    while falling or filling:
        while falling:
            x, y = falling.pop()

            while y < ymax:
                y += 1
                if clay[(x, y)]:
                    filling.add((x, y - 1))
                    break
                else:
                    flowing.add((x, y))

        while filling:
            x, y = filling.pop()
            fills = set()

            xl, yl = x, y
            sl = None
            while not sl and not clay[(xl, yl)]:
                fills.add((xl, yl))

                if not clay[(xl, yl + 1)] and (xl, yl + 1) not in still:
                    sl = (xl, yl)
                    falling.add(sl)

                xl -= 1

            xr, yr = x, y
            sr = None
            while not sr and not clay[(xr, yr)]:
                fills.add((xr, yr))

                if not clay[(xr, yr + 1)] and (xr, yr + 1) not in still:
                    sr = (xr, yr)
                    falling.add(sr)

                xr += 1

            if not sl and not sr:
                still.update(fills)
                filling.add((x, y - 1))
            else:
                flowing.update(fills)

    part1 = sum([p[1] >= ymin for p in (flowing | still)])
    part2 = sum([p[1] >= ymin for p in still])

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
