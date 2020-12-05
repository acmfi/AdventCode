#!/usr/bin/env python3

import sys


def solve(s):
    score = 0
    garbage_count = 0
    garbage = False
    group_score = 0
    group = []

    idx = 0

    while idx < len(s):
        if s[idx] == "!":
            idx += 1
        elif garbage and s[idx] == ">":
            garbage = False
        elif garbage:
            garbage_count += 1
        elif s[idx] == "<":
            garbage = True
        elif s[idx] == "{":
            group_score += 1
            group.append(group_score)
        elif s[idx] == "}":
            group_score -= 1
            score += group.pop()
        idx += 1

    return score, garbage_count


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)

    s = None
    with open(sys.argv[1], 'r') as f:
        s = f.readlines()[0]

    part_1, part_2 = solve(s)
    print("Part 1: {}".format(part_1))
    print("Part 2: {}".format(part_2))
