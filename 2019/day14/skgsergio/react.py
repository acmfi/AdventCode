#!/usr/bin/env python3
import sys
import math

from typing import List, Dict, Tuple


def part1(data: Dict[str, Tuple[int, List[Tuple[int, str]]]],
          pqty: int, pwhat: str, rwhat: str) -> int:
    req = {pwhat: pqty}

    while True:
        try:
            nw, nqty = next((w, qty) for w, qty in req.items() if qty > 0 and w != rwhat)
        except StopIteration:
            break

        pqty, reqs = data[nw]
        factor = math.ceil(nqty / pqty)

        for qty, w in reqs:
            req[w] = req.get(w, 0) + (factor * qty)

        req[nw] -= factor * pqty

    return req[rwhat]


def part2(data: Dict[str, Tuple[int, List[Tuple[int, str]]]], available: int,
          pqty: int, pwhat: str, rwhat: str) -> int:
    minf = available // part1(r, 1, 'FUEL', 'ORE')
    maxf = 2 * available

    while (maxf - minf) > 1:
        fuel = (maxf + minf) // 2

        if part1(r, fuel, 'FUEL', 'ORE') > available:
            maxf = fuel
        else:
            minf = fuel

    return minf


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    r = {}
    with open(sys.argv[1], 'r') as f:
        for l in f:
            source, product = l.strip().split(" => ")
            pqty, pname = product.split(" ")

            r[pname] = (int(pqty), [(int(s.split(" ")[0]), s.split(" ")[1]) for s in source.split(", ")])

    print(f"Part 1: {part1(r, 1, 'FUEL', 'ORE')}")
    print(f"Part 2: {part2(r, 1000000000000, 1, 'FUEL', 'ORE')}")
