#!/usr/bin/env python3
import sys

import re
from datetime import datetime
from collections import defaultdict


def solve(d):
    log = sorted([(datetime.strptime(r.groups()[0], "%Y-%m-%d %H:%M"), r.groups()[1].lower())
                  for r in re.finditer(r"\[(.*)\] (.*)", d)], key=lambda x: x[0])

    gs = defaultdict(list)
    ts = defaultdict(int)
    for t, a in log:
        if a.split()[0] == 'guard':
            g = int(a.split()[1][1:])
        if a == 'falls asleep':
            s = t
        elif a == 'wakes up':
            gs[g].append((s.minute, t.minute))
            ts[g] += (t - s).seconds

    g = max(ts, key=ts.get)
    m, _ = max([(m, sum(s <= m < e for s, e in gs[g])) for m in range(60)], key=lambda x: x[1])
    part1 = g * m

    g, m, _ = max([(g, m, sum(s <= m < e for s, e in gs[g])) for g in gs for m in range(60)], key=lambda x: x[2])
    part2 = g * m

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read()

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
