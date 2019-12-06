#!/usr/bin/env python3
import sys


from collections import defaultdict
from typing import List, Dict


def part1(data: List[List[str]]) -> int:
    omap: Dict[str, List[str]] = defaultdict(list)
    for o1, o2 in data:
        omap[o1].append(o2)

    orbits = {
        'COM': 0
    }

    while omap:
        for obj in omap:
            if obj in orbits:
                for objorb in omap[obj]:
                    orbits[objorb] = orbits[obj] + 1
                del omap[obj]
                break

    return sum(orbits.values())


def part2(data: List[List[str]]) -> int:
    return 0


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = []
    with open(sys.argv[1], 'r') as f:
        for l in f:
            d.append(l.strip().split(')'))

    print(f"Part 1: {part1(d)}")
    print(f"Part 2: {part2(d)}")
