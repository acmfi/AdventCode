#!/usr/bin/env python3
import sys


def part1(data: str, w: int=25, h: int=6) -> int:
    layers = [data[i:i + w * h] for i in range(0, len(data), w * h)]

    layer = min(layers, key=lambda l: l.count('0'))

    return layer.count('1') * layer.count('2')


def part2(data: str, w: int=25, h: int=6) -> str:
    layers = [data[i:i + w * h] for i in range(0, len(data), w * h)]

    pixels = ''.join(''.join(pstack).lstrip('2')[:1] for pstack in zip(*layers))

    imgchr = str.maketrans('012', ' #.')
    imglines = [pixels[i * w:(i + 1) * w].translate(imgchr) for i in range(h)]

    return '\n'.join(imglines)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        d = f.read().splitlines()[0]

    print(f"Part 1: {part1(d)}")
    print(f"Part 2:\n{part2(d)}")
