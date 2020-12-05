#!/usr/bin/env python3

import sys


def part_1_is_valid_pwd(pwd):
    pwd_l = pwd.split()

    if len(pwd_l) != len(set(pwd_l)):
        return False
    else:
        return True


def part_2_is_valid_pwd(pwd):
    pwd_l = pwd.split()

    if len(pwd_l) != len(set(''.join(sorted(w)) for w in pwd_l)):
        return False
    else:
        return True


def check_pwds(pwd_list):
    part_1_valids = 0
    part_2_valids = 0

    for pwd in pwd_list:
        if part_1_is_valid_pwd(pwd):
            part_1_valids += 1

        if part_2_is_valid_pwd(pwd):
            part_2_valids += 1

    return part_1_valids, part_2_valids


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: {} <input_list>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    pwd_list = []
    with open(sys.argv[1], 'r') as f:
        for pwd in f:
            pwd_list.append(pwd)

    part_1_valids, part_2_valids = check_pwds(pwd_list)
    print("Valids passwords (part 1): {}".format(part_1_valids))
    print("Valids passwords (part 2): {}".format(part_2_valids))
