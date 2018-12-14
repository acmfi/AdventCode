#!/usr/bin/env python3
import sys


def solve(n):
    ds = [int(d) for d in str(n)]
    sc = [3, 7]
    e1 = 0
    e2 = 1

    part1 = None
    part2 = None

    while not part1 or not part2:
        t = sc[e1] + sc[e2]
        sc.extend(divmod(t, 10) if t >= 10 else [t])

        e1 = (e1 + 1 + sc[e1]) % len(sc)
        e2 = (e2 + 1 + sc[e2]) % len(sc)

        if len(sc) == n + 10:
            part1 = ''.join(map(str, sc[n:n+10]))

        if sc[-len(ds):] == ds or sc[-len(ds)-1:-1] == ds:
            part2 = len(sc) - len(ds) - (sc[-len(ds)-1:-1] == ds)

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>\n\tMy input: 637061",
              file=sys.stderr)
        exit(1)

    part1, part2 = solve(int(sys.argv[1]))

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
