#!/usr/bin/env python3
import sys

from typing import List, Tuple, Set


def adjacents(land: List[str], p: Tuple[int, int]) -> int:
    count = 0

    for x, y in [(p[0], p[1] + 1), (p[0], p[1] - 1),
                 (p[0] + 1, p[1]), (p[0] - 1, p[1])]:

        if y not in range(0, len(land)) or x not in range(0, len(land[y])):
            continue

        if land[y][x] == "#":
            count += 1

    return count


def cycle(land: List[str]) -> List[str]:
    new_land = [["."] * len(land[y]) for y in range(len(land))]

    for y in range(len(land)):
        for x in range(len(land[y])):
            a = adjacents(land, (x, y))

            if land[y][x] == "#":
                if a == 1:
                    new_land[y][x] = "#"

            elif a == 1 or a == 2:
                new_land[y][x] = "#"

    return ["".join(l) for l in new_land]


def biorating(land: List[str]) -> int:
    result = 0
    power = 1

    for y in range(len(land)):
        for x in range(len(land[y])):
            if land[y][x] == "#":
                result += power

            power *= 2

    return result


def part1(data: List[str]) -> int:
    land = data.copy()
    rating = biorating(land)
    ratings: Set[int] = set()

    while rating not in ratings:
        ratings.add(rating)
        land = cycle(land)
        rating = biorating(land)

    return rating


def adjacents2(land: List[List[List[str]]], level: int, p: Tuple[int, int]) -> int:
    count = 0

    for x, y in [(p[0], p[1] + 1), (p[0], p[1] - 1),
                 (p[0] + 1, p[1]), (p[0] - 1, p[1])]:

        if (x, y) == (2, 2):
            if p[1] == 1:
                count += sum(land[level - 1][0][i] == "#" for i in range(len(land[level - 1][0])))

            elif p[1] == 3:
                count += sum(land[level - 1][4][i] == "#" for i in range(len(land[level - 1][4])))

            elif p[0] == 1:
                count += sum(land[level - 1][j][0] == "#" for j in range(len(land[level - 1])))

            elif p[0] == 3:
                count += sum(land[level - 1][j][4] == "#" for j in range(len(land[level - 1])))

        elif y == -1:
            if land[level + 1][1][2] == "#":
                count += 1

        elif y == 5:
            if land[level + 1][3][2] == "#":
                count += 1

        elif x == -1:
            if land[level + 1][2][1] == "#":
                count += 1

        elif x == 5:
            if land[level + 1][2][3] == "#":
                count += 1

        elif land[level][y][x] == "#":
            count += 1

    return count


def cycle2(land: List[List[List[str]]], its: int) -> List[List[List[str]]]:
    new_land = [[["."] * len(land[0][y]) for y in range(len(land[0]))] for _ in range((its * 2) + 3)]

    for level in range((its * 2) + 1):
        for y in range(len(land[level])):
            for x in range(len(land[level][y])):
                if (x, y) == (2, 2):
                    continue

                a = adjacents2(land, level, (x, y))

                if land[level][y][x] == "#":
                    if a == 1:
                        new_land[level][y][x] = "#"

                else:
                    if a == 1 or a == 2:
                        new_land[level][y][x] = "#"

    return new_land


def part2(data: List[str]) -> int:
    its = 200
    lvls = (its * 2) + 3
    land = [[["." for x in range(len(data[y]))] for y in range(len(data))] for _ in range(lvls)]
    land[its + 1] = data.copy()

    for _ in range(its):
        land = cycle2(land, its)

    return sum(land[lvl][y][x] == "#" for lvl in range(lvls) for y in range(len(land[lvl])) for x in range(len(land[lvl][y])))


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    print(f"Part 1: {part1(d)}")
    print(f"Part 2: {part2(d)}")
