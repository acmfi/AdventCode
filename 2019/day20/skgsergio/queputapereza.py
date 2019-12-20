#!/usr/bin/env python3
import sys
import string

from typing import List, Dict, Tuple, Set


def parse_this_shit(data: List[str]) -> Tuple[Set[Tuple[int, int]], Dict[str, List[Tuple[int, int]]]]:
    ylen = len(data)
    xlen = len(data[0])

    tiles: Set[Tuple[int, int]] = set()
    portals: Dict[str, List[Tuple[int, int]]] = {}

    for y in range(ylen):
        for x in range(xlen):
            if data[y][x] == '.':
                tiles.add((y, x))

            elif data[y][x] in string.ascii_uppercase:
                if y < (ylen - 1) and data[y + 1][x] in string.ascii_uppercase:
                    t = (y - 1, x) if data[y - 1][x] == '.' else (y + 2, x)

                    if data[y][x] + data[y + 1][x] in portals:
                        portals[data[y][x] + data[y + 1][x]].append(t)

                    else:
                        portals[data[y][x] + data[y + 1][x]] = [t]

                elif x < (xlen - 1) and data[y][x + 1] in string.ascii_uppercase:
                    t = (y, x - 1) if data[y][x - 1] == '.' else (y, x + 2)

                    if data[y][x] + data[y][x + 1] in portals:
                        portals[data[y][x] + data[y][x + 1]].append(t)

                    else:
                        portals[data[y][x] + data[y][x + 1]] = [t]

    return tiles, portals


def generate_shitty_graph_p1(tiles: Set[Tuple[int, int]], portals: Dict[str, List[Tuple[int, int]]]) -> Dict[Tuple[int, int], List[Tuple[int, int]]]:
    graph = {}

    for n in tiles:
        ns = []

        for pn in [(n[0] + 1, n[1]), (n[0], n[1] - 1),
                   (n[0] - 1, n[1]), (n[0], n[1] + 1)]:

            if pn in tiles:
                ns.append(pn)

        for p in portals.values():
            if len(p) == 2 and n in p:
                ns.append(p[(p.index(n) + 1) % 2])

        graph[n] = ns

    return graph


def bfs_p1(graph: Dict[Tuple[int, int], List[Tuple[int, int]]], start: Tuple[int, int], end: Tuple[int, int]) -> int:
    seen = set([start])
    queue = [(start, 0)]

    while queue:
        v = queue.pop(0)

        if v[0] == end:
            break

        for n in graph[v[0]]:
            if n not in seen:
                queue.append((n, v[1] + 1))
                seen.add(n)

    return v[1]


def generate_shitty_graph_p2(tiles, portals, leny, lenx):
    graph = {}

    for n in tiles:
        ns = []

        for pn in [(n[0] + 1, n[1]), (n[0], n[1] - 1),
                   (n[0] - 1, n[1]), (n[0], n[1] + 1)]:

            if pn in tiles:
                ns.append((pn, 0))

        for p in portals.values():
            if len(p) == 2 and n in p:
                if n[0] in [2, leny - 3] or n[1] in [2, lenx - 3]:
                    ns.append((p[(p.index(n) + 1) % 2], -1))

                else:
                    ns.append((p[(p.index(n) + 1) % 2], 1))

        graph[n] = ns

    return graph


def bfs_p2(graph: Dict[Tuple[int, int], List[Tuple[int, int]]], start: Tuple[int, int], end: Tuple[int, int]) -> int:
    seen = set([(start, 0)])
    queue = [(start, 0, 0)]

    while queue:
        v = queue.pop(0)

        if v[0] == end and v[1] == 0:
            break

        for n in graph[v[0]]:
            if v[1] + n[1] < 0:
                continue

            if (n[0], v[1] + n[1]) not in seen:
                queue.append((n[0], v[1] + n[1], v[2] + 1))
                seen.add((n[0], v[1] + n[1]))

    return v[2]


def solve(data: List[str], start: str, end: str) -> Tuple[int, int]:
    tiles, portals = parse_this_shit(data)

    graph_p1 = generate_shitty_graph_p1(tiles, portals)
    p1 = bfs_p1(graph_p1, portals[start][0], portals[end][0])

    graph_p2 = generate_shitty_graph_p2(tiles, portals, len(data), len(data[0]))
    p2 = bfs_p2(graph_p2, portals[start][0], portals[end][0])

    return p1, p2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()

    part1, part2 = solve(d, 'AA', 'ZZ')

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
