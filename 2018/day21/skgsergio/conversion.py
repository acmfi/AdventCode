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
    part1 = None
    part2 = None

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

    # Part 1
    R = [0] * 6

    while 0 <= R[ip_reg] < len(program):
        ins = program[R[ip_reg]]

        if ins['op'] == 'eqrr':
            part1 = R[ins['A']]
            break

        R[ins['C']] = OPS[ins['op']](R, ins['A'], ins['B'])
        R[ip_reg] += 1

    # Part 2: Let's run it I don't even care enough to reverse the code
    seen = set()
    last = 0
    R = [0] * 6

    while 0 <= R[ip_reg] < len(program):
        ins = program[R[ip_reg]]

        if ins['op'] == 'eqrr':
            val = R[ins['A']]

            if val in seen:
                part2 = last
                break
            else:
                last = val
                seen.add(val)

        R[ins['C']] = OPS[ins['op']](R, ins['A'], ins['B'])
        R[ip_reg] += 1

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
