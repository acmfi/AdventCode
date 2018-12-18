#!/usr/bin/env python3
import sys


NEIGHBORS = [(x, y) for x in range(-1, 2) for y in range(-1, 2) if (x, y) != (0, 0)]


def iteration(area):
    xl = len(area)
    yl = len(area[0])

    res = [['.'] * yl for _ in range(xl)]

    for x in range(xl):
        for y in range(yl):
            c = {'.': 0, '|': 0, '#': 0}

            for dx, dy in NEIGHBORS:
                if 0 <= x+dx < xl and 0 <= y+dy < yl:
                    c[area[x+dx][y+dy]] += 1

            if area[x][y] == '.' and c['|'] >= 3:
                res[x][y] = '|'

            elif area[x][y] == '#' and c['#'] >= 1 and c['|'] >= 1:
                res[x][y] = '#'

            elif area[x][y] == '|':
                res[x][y] = '#' if c['#'] >= 3 else '|'

    return res


def solve(area):
    part1 = None
    part2 = None

    it = 0
    saves = []
    while not part1 or not part2:
        area = iteration(area)
        res = ''.join([''.join(x) for x in area])

        if not part1 and (it + 1) == 10:
            part1 = res.count('#') * res.count('|')

        if res in saves:
            idx = saves.index(res)
            per = it - idx
            target = (1000000000 % per)

            while (idx + 1) % per != target:
                idx += 1

            part2 = saves[idx].count('#') * saves[idx].count('|')
        else:
            saves.append(res)

        it += 1

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
