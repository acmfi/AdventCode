#!/usr/bin/env python3
import sys

from typing import List, Dict, Tuple, FrozenSet


KEYS = "abcdefghijklmnopqrstuvwxyz"
PASSAGES = "."
ENTRANCES = "@$%&"
WALLS = "#"


def distances(maze: List[str], start: Tuple[int, int]) -> Dict[str, Tuple[int, str]]:
    sx, sy = start

    queue = [((sx, sy), 0, "")]
    visited = set([(sx, sy)])

    routes = {}

    for (x, y), dist, route in queue:
        v = maze[y][x]

        if v not in PASSAGES + ENTRANCES + WALLS and dist > 0:
            routes[v] = (dist, route)
            route = route + v

        visited.add((x, y))

        for nx, ny in [(x + 1, y), (x - 1, y),
                       (x, y + 1), (x, y - 1)]:

            if maze[ny][nx] not in WALLS and (nx, ny) not in visited:
                queue.append(((nx, ny), dist+1, route))

    return routes


def get_routes(data: List[str]) -> Dict[str, Dict[str, Tuple[int, str]]]:
    routes = {}

    for y in range(len(data)):
        for x in range(len(data[0])):
            if data[y][x] in KEYS + ENTRANCES:
                routes[data[y][x]] = distances(data, (x, y))

    return routes


def part1(data: List[str]) -> int:
    routes = get_routes(data)
    keys = frozenset(k for k in routes.keys() if k in KEYS)

    paths: Dict[Tuple[str, FrozenSet[str]], int] = {
        (ENTRANCES[0], frozenset()): 0
    }

    for _ in range(len(keys)):
        npaths: Dict[Tuple[str, FrozenSet[str]], int] = {}

        for (cloc, ckeys), cdist in paths.items():
            for nkey in keys:
                if nkey not in ckeys:
                    dist, route = routes[cloc][nkey]

                    if all((c in ckeys or c.lower() in ckeys) for c in route):
                        ndist = cdist + dist
                        nkeys = ckeys | {nkey}

                        if (nkey, nkeys) not in npaths or ndist < npaths[(nkey, nkeys)]:
                            npaths[(nkey, nkeys)] = ndist
        paths = npaths

    return min(paths.values())


def update_map(data: List[str]) -> List[str]:
    ldata = [list(l) for l in data]

    for y in range(len(data)):
        for x in range(len(data[0])):
            if ldata[y][x] == ENTRANCES[0]:
                for wx, wy in [(x, y), (x + 1, y), (x - 1, y),
                               (x, y + 1), (x, y - 1)]:
                    ldata[wy][wx] = WALLS

                ldata[y + 1][x + 1] = ENTRANCES[0]
                ldata[y - 1][x + 1] = ENTRANCES[1]
                ldata[y + 1][x - 1] = ENTRANCES[2]
                ldata[y - 1][x - 1] = ENTRANCES[3]

                return ["".join(l) for l in ldata]

    raise Exception("Entrance not found")


def part2(data: List[str]) -> int:
    data = update_map(data)

    routes = get_routes(data)
    keys = frozenset(k for k in routes.keys() if k in KEYS)

    paths: Dict[Tuple[Tuple[str, ...], FrozenSet[str]], int] = {
        (tuple(ENTRANCES), frozenset()): 0
    }

    for _ in range(len(keys)):
        npaths: Dict[Tuple[Tuple[str, ...], FrozenSet[str]], int] = {}

        for (clocs, ckeys), cdist in paths.items():
            for nkey in keys:
                if nkey not in ckeys:
                    for robot in range(4):
                        if nkey in routes[clocs[robot]]:
                            dist, route = routes[clocs[robot]][nkey]

                            if all((c in ckeys or c.lower() in ckeys) for c in route):
                                ndist = cdist + dist
                                nkeys = ckeys | {nkey}

                                nlocs = clocs[:robot] + tuple(nkey) + clocs[robot + 1:]

                                if (nlocs, nkeys) not in npaths or ndist < npaths[(nlocs, nkeys)]:
                                    npaths[(nlocs, nkeys)] = ndist
        paths = npaths

    return min(paths.values())


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    print(f"Part 1: {part1(d)}")
    print(f"Part 2: {part2(d)}")
