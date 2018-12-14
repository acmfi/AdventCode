#!/usr/bin/env python3
import sys


def solve(d):
    sc = [3, 7]
    e1 = 0
    e2 = 1

    while len(sc) < d + 10:
        t = sc[e1] + sc[e2]
        sc.extend(divmod(t, 10) if t >= 10 else [t])

        e1 = (e1 + 1 + sc[e1]) % len(sc)
        e2 = (e2 + 1 + sc[e2]) % len(sc)

    part1 = ''.join(map(str, sc[d:d+10]))

    part2 = None

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>\n\tMy input: 637061".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    part1, part2 = solve(int(sys.argv[1]))

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
