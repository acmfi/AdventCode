#!/usr/bin/env python3
import sys

from typing import List


DEBUG = False


class Intcode:
    def __init__(self, program: List[int], input_values: List[int],
                 pc: int = 0, output_halts: bool = False):
        self._d = program[:]
        self._inputs = input_values
        self._output_halts = output_halts

        self._eop = False
        self._pc = pc
        self._rb = 0
        self._m1 = 0
        self._m2 = 0
        self._m3 = 0
        self._result = -1

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
        if len(self._d) <= i:
            self._d.extend([0] * (i + 1 - len(self._d)))

    def _get(self, i: int, immediate_mode: int) -> int:
        if immediate_mode == 0:
            value = self._d[self._d[self._pc + i]]

        elif immediate_mode == 1:
            value = self._d[self._pc + i]

        elif immediate_mode == 2:
            value = self._d[self._rb + self._d[self._pc + i]]

        else:
            raise Exception(f"[PC:{self._pc}] Invalid mode '{immediate_mode}'")

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
        self._result = self._get(1, self._m1)
        self._pc += 2
        self._eop = self._output_halts

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
            self._op_table[self._op]()
        else:
            self._op_hcf()

    def run(self) -> int:
        if self._eop:
            return self._result

        while not self._eop:
            self._decode()
            self._exec()

        return self._result


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        data = f.read()

    program = list(map(int, data.split(',')))

    print(f"Part 1: {Intcode(program, [1]).run()}")
    print(f"Part 2: {Intcode(program, [2]).run()}")
