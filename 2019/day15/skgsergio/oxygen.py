#!/usr/bin/env python3
import sys
import random

from typing import List, Dict, Tuple, Optional


DEBUG = False


class Intcode:
    def __init__(self, program: List[int]):
        self._d = program[:]

        self._eop = False
        self._pc = 0
        self._rb = 0
        self._m1 = 0
        self._m2 = 0
        self._m3 = 0
        self._output = None

        self._op_table = {
            1: self._op_add,
            2: self._op_mul,
            3: self._op_in,
            4: self._op_out,
            5: self._op_jit,
            6: self._op_jif,
            7: self._op_lt,
            8: self._op_eq,
            9: self._op_arb,
            99: self._op_eop,
        }

    # Decode functions
    def _digit(self, value: int, i: int) -> int:
        return (value // (10 ** i)) % 10

    def _decode(self):
        instruction = self._d[self._pc]

        self._op = instruction % 100
        self._m1 = self._digit(instruction, 2)
        self._m2 = self._digit(instruction, 3)
        self._m3 = self._digit(instruction, 4)

        if DEBUG:
            print(f"[DEBUG:_decode] ins: {instruction}, op: {self._op}, m: [{self._m1},{self._m2},{self._m3}]")

    # Memory access functions
    def _check_mem_size(self, i: int):
        if DEBUG:
            print(f"[DEBUG:_check_mem_size] i: {i}, len(d): {len(self._d)}")

        if len(self._d) <= i:
            self._d.extend([0] * (i + 1 - len(self._d)))

    def _get(self, i: int, immediate_mode: int) -> int:
        if immediate_mode == 0:
            idx = self._d[self._pc + i]

        elif immediate_mode == 1:
            idx = self._pc + i

        elif immediate_mode == 2:
            idx = self._rb + self._d[self._pc + i]

        else:
            raise Exception(f"[PC:{self._pc}] Invalid mode '{immediate_mode}'")

        self._check_mem_size(idx)

        value = self._d[idx]

        if DEBUG:
            print(f"[DEBUG:_get] i: {i}, immediate_mode: {immediate_mode}, value: {value}")

        return value

    def _set(self, i: int, immediate_mode: int, value: int):
        if DEBUG:
            print(f"[DEBUG:_set] i: {i}, immediate_mode: {immediate_mode}, value: {value}")

        if immediate_mode == 0:
            idx = self._d[self._pc + i]

        elif immediate_mode == 1:
            idx = self._pc + i

        elif immediate_mode == 2:
            idx = self._rb + self._d[self._pc + i]

        else:
            raise Exception(f"[PC:{self._pc}] Invalid mode '{immediate_mode}'")

        self._check_mem_size(idx)

        self._d[idx] = value

    # Operation functions
    def _op_add(self):
        self._set(3, self._m3, self._get(1, self._m1) + self._get(2, self._m2))
        self._pc += 4

    def _op_mul(self):
        self._set(3, self._m3, self._get(1, self._m1) * self._get(2, self._m2))
        self._pc += 4

    def _op_in(self):
        self._set(1, self._m1, self._inputs.pop(0))
        self._pc += 2

    def _op_out(self):
        self._output = self._get(1, self._m1)
        self._pc += 2

    def _op_jit(self):
        if self._get(1, self._m1) != 0:
            self._pc = self._get(2, self._m2)
        else:
            self._pc += 3

    def _op_jif(self):
        if self._get(1, self._m1) == 0:
            self._pc = self._get(2, self._m2)
        else:
            self._pc += 3

    def _op_lt(self):
        self._set(3, self._m3, self._get(1, self._m1) < self._get(2, self._m2))
        self._pc += 4

    def _op_eq(self):
        self._set(3, self._m3, self._get(1, self._m1) == self._get(2, self._m2))
        self._pc += 4

    def _op_arb(self):
        self._rb += self._get(1, self._m1)
        self._pc += 2

    def _op_eop(self):
        self._eop = True

    def _op_hcf(self):
        raise Exception(f"[PC:{self._pc}] Invalid operation '{self._op}'")

    # Exec functions
    def _exec(self):
        if self._op in self._op_table:
            if DEBUG:
                print(f"[DEBUG:_exec] op: {self._op}, fn: {self._op_table[self._op].__name__}")

            self._op_table[self._op]()

        else:
            self._op_hcf()

    def run(self, input_values: List[int]) -> Optional[int]:
        if self._eop:
            raise Exception(f"[EOP] Machine already in End Of Program state.")

        self._inputs = input_values
        self._output = None
        self._halt = False

        while not self._eop and self._output is None:
            self._decode()
            self._exec()

        return self._output


def brute_path(data: List[int]) -> Dict[Tuple[int, int], int]:
    board: Dict[Tuple[int, int], int] = {}

    dirs = [
        (1, 0),
        (-1, 0),
        (0, -1),
        (0, 1)
    ]

    m = Intcode(data)

    x, y = 0, 0
    for c in range(1000000):
        nxt = random.choice(range(4))

        while board.get((x + dirs[nxt][0], y + dirs[nxt][1]), 1) == 0:
            nxt = random.choice(range(4))

        out = m.run([nxt + 1])

        if out is None or m._eop:
            break

        newx = x + dirs[nxt][0]
        newy = y + dirs[nxt][1]

        board[(newx, newy)] = out

        if out != 0:
            x = newx
            y = newy

    return board


def bfs(graph, start, end=None):
    dist = -1

    seen = set()
    queue = [start]

    while queue:
        nqueue = []
        dist += 1

        for p in queue:
            for n in [(p[0] + 1, p[1]), (p[0] - 1, p[1]),
                      (p[0], p[1] + 1), (p[0], p[1] - 1)]:
                if graph.get(n, 1) == 0:
                    continue

                if end is not None and n == end:
                    return dist + 1

                if n not in seen:
                    nqueue.append(n)

                seen.add(n)

        queue = nqueue

    return dist


def solve(data: List[int]) -> Tuple[int, int]:
    board = brute_path(data)
    oxygen = next(k for k, v in board.items() if v == 2)

    p1 = bfs(board, (0, 0), oxygen)
    p2 = bfs(board, oxygen)

    return p1, p2


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input> [print]", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        data = f.read()

    program = list(map(int, data.split(',')))

    part1, part2 = solve(program)

    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")
