#!/usr/bin/env python3

import sys


def part_1_is_valid_pwd(pwd):
    pwd_l = pwd.split()

    if len(pwd_l) != len(set(pwd_l)):
        return False
    else:
        return True


def part_1(pwd_list):
    valids = 0

    for pwd in pwd_list:
        if part_1_is_valid_pwd(pwd):
            valids += 1

    return valids


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: {} <input_list>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    pwd_list = []
    with open(sys.argv[1], 'r') as f:
        for pwd in f:
            pwd_list.append(pwd)

    print("Valids passwords: {}".format(part_1(pwd_list)))
