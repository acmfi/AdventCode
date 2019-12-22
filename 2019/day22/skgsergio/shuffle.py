#!/usr/bin/env python3
import sys

from enum import Enum

from typing import List, Tuple


class Action(Enum):
    DEAL_WITH = 0
    DEAL_INTO = 1
    CUT = 2


def part1(data: List[Tuple[Action, int]]) -> int:
    dlen = 10007
    deck = list(range(dlen))

    for t in data:
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

    return deck.index(2019)


def part2(data: List[Tuple[Action, int]]) -> int:
    """
    DISCLAMER:

    My first attempt result was the following:

    pypy3(29530,0x10a32ad40) malloc: can't allocate region
    *** mach_vm_map(size=954525740113920) failed (error code=3)
    pypy3(29530,0x10a32ad40) malloc: *** set a breakpoint in malloc_error_break to debug

    My second attempt resulted in a code that was running (and never ended,
    it ran for more than 50 minutes) while I looked for inspiration as I knew
    this approach wasn't going to work.

    The following approach is ~stolen~ inspired from the following AoC Reddit
    solution thread comment, I did not have a look to the code, although it was
    conveniently coded in Python, since the comment is enough to understand the
    problem and the solution he came with.

    https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbnkaju/

    All the credit goes to /r/mcpower_ (https://github.com/mcpower/)

    I don't deserve the star :(
    """
    deck = 119315717514047
    iterations = 101741582076661

    imult = 1
    odiff = 0

    for t in data:
        if t[0] == Action.DEAL_INTO:
            imult = (imult * -1) % deck
            odiff = (odiff + imult) % deck

        elif t[0] == Action.CUT:
            odiff = (odiff + (t[1] * imult)) % deck

        elif t[0] == Action.DEAL_WITH:
            imult = (imult * pow(t[1], deck - 2, deck)) % deck

        else:
            raise Exception(f"Unknown action: {t[0]}")


    increment = pow(imult, iterations, deck)
    offset = (odiff * (1 - increment) * pow((1 - imult) % deck, deck - 2, deck)) % deck

    return (offset + 2020 * increment) % deck


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
