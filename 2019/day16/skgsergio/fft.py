#!/usr/bin/env python3
import sys

from typing import List


def fft(data: List[int], offset: int = 0) -> List[int]:
    result = [0] * len(data)

    if offset > len(data) // 2:
        dsum = sum(data[offset:])

        for i in range(offset, len(data)):
            result[i] = dsum % 10
            dsum -= data[i]

    else:
        for i in range(offset, len(data)):
            for s, j in enumerate(range(i, len(data), (i + 1) * 2)):
                result[i] += sum(data[j:j + i + 1]) * (-1 if s % 2 else 1)

            result[i] = abs(result[i]) % 10

    return result


def part1(data: List[int]) -> int:
    data = data.copy()

    for _ in range(100):
        data = fft(data)

    return int("".join(map(str, data[:8])))


def part2(data: List[int]) -> int:
    offset = int("".join(map(str, data[:7])))
    data = data * 10000

    for _ in range(100):
        data = fft(data, offset)

    return int("".join(map(str, data[offset:offset + 8])))


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        d = list(map(int, f.read().strip()))

    print(f"Part 1: {part1(d)}")
    print(f"Part 2: {part2(d)}")
