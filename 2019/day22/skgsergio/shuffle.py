#!/usr/bin/env python3
import sys

from enum import Enum

from typing import List, Tuple


class Action(Enum):
    DEAL_WITH = 0
    DEAL_INTO = 1
    CUT = 2


def shuffle(deck, techniques):
    dlen = len(deck)

    for t in techniques:
        if t[0] == Action.DEAL_INTO:
            deck.reverse()

        elif t[0] == Action.CUT:
            upper = deck[t[1]:]
            lower = deck[:t[1]]

            deck = upper + lower

        elif t[0] == Action.DEAL_WITH:
            nd = [0] * dlen

            for i in range(dlen):
                nd[(i * t[1]) % dlen] = deck[i]

            deck = nd

        else:
            raise Exception(f"Unknown action: {t[0]}")

    return deck


def part1(data: List[Tuple[Action, int]]) -> int:
    deck = list(range(10007))

    result = shuffle(deck, data)

    return result.index(2019)


def part2(data: List[Tuple[Action, int]]) -> int:
    return 0


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = []
    with open(sys.argv[1], 'r') as f:
        for line in f:
            if line.startswith("deal into new stack"):
                d.append((Action.DEAL_INTO, 0))

            elif line.startswith("cut "):
                d.append((Action.CUT, int(line.split()[-1])))

            elif line.startswith("deal with increment "):
                d.append((Action.DEAL_WITH, int(line.split()[-1])))

            else:
                raise Exception(f"Unknown techinique: {line}")

    print(f"Part 1: {part1(d)}")
    print(f"Part 2: {part2(d)}")
