#!/usr/bin/env python3
import sys

from collections import defaultdict


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


def get_valid_ops(s):
    valid = set()

    for op in OPS.keys():
        res = s['before'][:]
        res[s['C']] = OPS[op](s['before'], s['A'], s['B'])

        if res == s['after']:
            valid.add(op)

    return valid


def solve(d):
    # Process this shitty multipart input
    input_parts = d.split('\n\n\n\n')
    part1_input = [l for l in input_parts[0].splitlines() if l]
    part2_input = input_parts[1].splitlines()

    # Parse part 1 input
    samples = []
    for ln, l in enumerate(part1_input):
        lm = ln % 3
        if lm == 0:
            sample = {}
            sample['before'] = list(map(int, l.split(':')[1].strip()[1:-1].split(',')))
        elif lm == 1:
            sample['opcode'], sample['A'], sample['B'], sample['C'] = map(int, l.split())
        elif lm == 2:
            sample['after'] = list(map(int, l.split(':')[1].strip()[1:-1].split(',')))
            samples.append(sample)

    # Part 1: Number of samples matching to 3 or more ops
    part1 = sum(len(get_valid_ops(s)) >= 3 for s in samples)

    # Part 2: Use the samples to reverse the opcodes
    reverse_ops = {}

    i = 0
    while len(reverse_ops) < len(OPS):  # Iterate until we have all ops reversed
        # Just in case we need to do more than one round lets use the mod len(samples)
        s = samples[i % len(samples)]

        # If the opcode is not yet reversed try to do it with this sample
        if s['opcode'] not in reverse_ops:
            # Remove the already reversed from the valid ops for the sample
            ops = get_valid_ops(s) - set(reverse_ops.values())

            # If we only have one matching operation save it
            if len(ops) == 1:
                reverse_ops[s['opcode']] = ops.pop()

        i += 1

    # Run the part 2 input
    reg = defaultdict(int)
    for op in part2_input:
        opc, A, B, C = map(int, op.split())
        reg[C] = OPS[reverse_ops[opc]](reg, A, B)

    part2 = reg[0]

    return part1, part2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    d = None
    with open(sys.argv[1], 'r') as f:
        d = f.read()

    part1, part2 = solve(d)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
