#!/usr/bin/env python3
import sys

import copy
import string
from collections import defaultdict


def solve(d):
    g = defaultdict(set)

    for l in d:
        f = l.split()
        g[f[7]].add(f[1])

    o = []
    a = True
    while a:
        a = False
        for j in string.ascii_uppercase:
            if j not in o and g[j] & set(o) == g[j]:
                o.append(j)
                a = True
                break

    part1 = ''.join(o)

    t = 0
    st = set(o)
    rq = copy.deepcopy(g)
    wk = [[0, None] for i in range(5)]

    while st or any(w[0] > 0 for w in wk):
        w = list([s for s in st if all(s not in r for _, r in rq.items())])

        for i in range(5):
            wk[i][0] -= 1

            if wk[i][0] <= 0:
                if wk[i][1] in rq:
                    del rq[wk[i][1]]

                if w:
                    wk[i][1] = w.pop()
                    wk[i][0] = 60 + ord(wk[i][1]) - ord('A')
                    st.remove(wk[i][1])

        t += 1

    part2 = t

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
