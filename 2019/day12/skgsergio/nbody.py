#!/usr/bin/env python3
import sys
import re
import math

from copy import deepcopy
from itertools import combinations

from typing import List, Tuple, Set


INRE = re.compile(r'<x=(-?\d+),\s*y=(-?\d+),\s*z=(-?\d+)>')


class Moon:
    x: int
    y: int
    z: int
    vx: int = 0
    vy: int = 0
    vz: int = 0

    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def apply_velocity(self):
        self.x += self.vx
        self.y += self.vy
        self.z += self.vz

    def compare(self, other: 'Moon') -> Tuple[int, int, int]:
        cx, cy, cz = 0, 0, 0

        if self.x < other.x:
            cx = -1
        elif self.x > other.x:
            cx = 1

        if self.y < other.y:
            cy = -1
        elif self.y > other.y:
            cy = 1

        if self.z < other.z:
            cz = -1
        elif self.z > other.z:
            cz = 1

        return cx, cy, cz

    @property
    def potential_energy(self) -> int:
        return abs(self.x) + abs(self.y) + abs(self.z)

    @property
    def kinetic_energy(self) -> int:
        return abs(self.vx) + abs(self.vy) + abs(self.vz)

    @property
    def total_energy(self) -> int:
        return self.potential_energy * self.kinetic_energy


def step(moons: List[Moon]):
    for m1, m2 in combinations(moons, 2):
        cx, cy, cz = m1.compare(m2)

        m1.vx -= cx
        m1.vy -= cy
        m1.vz -= cz

        m2.vx += cx
        m2.vy += cy
        m2.vz += cz

    for m in moons:
        m.apply_velocity()


def part1(data: List[Moon], steps: int) -> int:
    moons = deepcopy(data)

    for _ in range(steps):
        step(moons)

    return sum(m.total_energy for m in moons)


def lcm(x: int, y: int) -> int:
    return (x * y) // math.gcd(x, y)


def part2(data: List[Moon]) -> int:
    moons = deepcopy(data)

    rx = 0
    ry = 0
    rz = 0

    c = 0
    seen: Set[str] = set()
    while not rx or not ry or not rz:
        step(moons)

        if not rx:
            x = '|'.join(f"x:{m.x},{m.vx}" for m in moons)

            if x in seen:
                rx = c

            seen.add(x)

        if not ry:
            y = '|'.join(f"y:{m.y},{m.vy}" for m in moons)

            if y in seen:
                ry = c

            seen.add(y)

        if not rz:
            z = '|'.join(f"z:{m.z},{m.vz}" for m in moons)

            if z in seen:
                rz = c

            seen.add(z)

        c += 1

    return lcm(lcm(rx, ry), rz)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = []
    with open(sys.argv[1], 'r') as f:
        for p in f:
            m = INRE.match(p)
            if m:
                d.append(Moon(*map(int, m.groups())))

    print(f"Part 1: {part1(d, 1000)}")
    print(f"Part 2: {part2(d)}")
