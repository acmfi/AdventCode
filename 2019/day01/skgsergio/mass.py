import sys

from typing import List


def module_fuel(mass: int) -> int:
    return (mass // 3) - 2


def required_fuel(masses: List[int]) -> int:
    return sum(module_fuel(m) for m in masses)


def total_fuel(masses: List[int]) -> int:
    total = 0

    for m in masses:
        while (m := module_fuel(m)) > 0:
            total += m

    return total


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        d = f.readlines()

    d = list(map(int, d))

    print(f'Part 1: {required_fuel(d)}')
    print(f'Part 2: {total_fuel(d)}')
