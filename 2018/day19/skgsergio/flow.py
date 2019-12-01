#!/usr/bin/env python3
import sys

OPS = {
    'addr': lambda R, a, b: R[a] + R[b],
    'addi': lambda R, a, b: R[a] + b,
    'mulr': lambda R, a, b: R[a] * R[b],
    'muli': lambda R, a, b: R[a] * b,
    'banr': lambda R, a, b: R[a] & R[b],
    'bani': lambda R, a, b: R[a] & b,
    'borr': lambda R, a, b: R[a] | R[b],
    'bori': lambda R, a, b: R[a] | b,
    'setr': lambda R, a, b: R[a],
    'seti': lambda R, a, b: a,
    'gtir': lambda R, a, b: int(a > R[b]),
    'gtri': lambda R, a, b: int(R[a] > b),
    'gtrr': lambda R, a, b: int(R[a] > R[b]),
    'eqir': lambda R, a, b: int(a == R[b]),
    'eqri': lambda R, a, b: int(R[a] == b),
    'eqrr': lambda R, a, b: int(R[a] == R[b]),
}


def solve(d):
    program = []
    ip_reg = None

    for ins in d:
        i = ins.split()

        if i[0] == '#ip':
            ip_reg = int(i[1])

        else:
            o = {'op': i[0]}
            o['A'], o['B'], o['C'] = map(int, i[1:])
            program.append(o)

    # Part 1: Just run the code
    R = [0] * 6

    while 0 <= R[ip_reg] < len(program):
        ins = program[R[ip_reg]]
        R[ins['C']] = OPS[ins['op']](R, ins['A'], ins['B'])
        R[ip_reg] += 1

    part1 = R[0]

    # Part 2: Looking the instructions it first calcs a number, then sets R[0]
    # to 0 (that is flag to say I'm done with the calc) and starts performing
    # the sum of factors for that number (including 1 and itself).
    # So just wait untill the flag is 0, check previous instruction register
    # destination, get the number and perform the same calc in a propper way xD
    R = [0] * 6
    R[0] = 1

    part2 = None
    while not part2:
        ins = program[R[ip_reg]]
        R[ins['C']] = OPS[ins['op']](R, ins['A'], ins['B'])
        R[ip_reg] += 1

        if R[0] == 0:
            n = R[program[R[ip_reg] - 2]['C']]
            part2 = sum(d for d in range(1, n + 1) if n % d == 0)

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
