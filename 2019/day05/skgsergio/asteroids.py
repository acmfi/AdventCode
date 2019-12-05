#!/usr/bin/env python3
import sys

from typing import List


class Intcode:
    def __init__(self, program: List[int], input_value: int):
        self._d = program[:]
        self._input = input_value
        self._pc = 0
        self._eop = False
        self._result = -1

    def _get(self, i: int, immediate_mode: int) -> int:
        value = self._d[self._pc + i]
        return value if immediate_mode else self._d[value]

    def _set(self, i: int, value: int):
        self._d[self._d[self._pc + i]] = value

    def _digit(self, value: int, i: int) -> int:
        return (value // (10 ** i)) % 10

    def run(self) -> int:
        if self._eop:
            return self._result

        while not self._eop:
            val = self._d[self._pc]

            m1 = self._digit(val, 2)
            m2 = self._digit(val, 3)

            op = (val % 100)

            if op == 99:  # EOP
                self._eop = True

            elif op == 1:  # +
                self._set(3, self._get(1, m1) + self._get(2, m2))
                self._pc += 4

            elif op == 2:  # *
                self._set(3, self._get(1, m1) * self._get(2, m2))
                self._pc += 4

            elif op == 3:  # IN
                self._set(1, self._input)
                self._pc += 2

            elif op == 4:  # OUT
                self._result = self._get(1, 0)
                self._pc += 2

            elif op == 5:  # JIT
                if self._get(1, m1) != 0:
                    self._pc = self._get(2, m2)
                else:
                    self._pc += 3

            elif op == 6:  # JIF
                if self._get(1, m1) == 0:
                    self._pc = self._get(2, m2)
                else:
                    self._pc += 3

            elif op == 7:  # LT
                self._set(3, self._get(1, m1) < self._get(2, m2))
                self._pc += 4

            elif op == 8:  # EQ
                self._set(3, self._get(1, m1) == self._get(2, m2))
                self._pc += 4

            else:
                raise Exception(f"[PC:{self._pc}] Invalid operation '{op}'")

        return self._result


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        data = f.read()

    program = list(map(int, data.split(',')))

    print(f"Part 1: {Intcode(program, 1).run()}")
    print(f"Part 2: {Intcode(program, 5).run()}")
