#!/usr/bin/env python3
import sys

import re
import matplotlib.pyplot as plt


rexp = re.compile(r'position=<\s*(-?\d+),\s*(-?\d+)>\s+velocity=<\s*(-?\d+),\s*(-?\d+)>')


def solve(d):
    pts = []

    for p in d:
        x, y, vx, vy = map(int, rexp.match(p).groups())
        pts.append({'x': x, 'y': y, 'vx': vx, 'vy': vy})

    bbx = float('inf')
    it = -1

    while True:
        x = [p['x'] + (p['vx'] * (it + 1)) for p in pts]
        y = [p['y'] + (p['vy'] * (it + 1)) for p in pts]

        nxt_bbx = (max(x) - min(x)) + (max(y) - min(y))

        if bbx < nxt_bbx:
            break

        else:
            bbx = nxt_bbx
            it += 1

    fig, ax = plt.subplots()
    ax.invert_yaxis()
    ax.set_facecolor('#0f0f23')
    ax.set_aspect(1)
    ax.set_title("seconds = {}".format(it))
    ax.plot([p['x'] + (p['vx'] * it) for p in pts],
            [p['y'] + (p['vy'] * it) for p in pts], '*', color='#ffff66')
    plt.show()


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    solve(d)
