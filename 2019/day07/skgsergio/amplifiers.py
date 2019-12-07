#!/usr/bin/env python3
import sys

from itertools import permutations

from typing import List


class Intcode:
    def __init__(self, program: List[int], input_values: List[int],
                 pc: int = 0, output_halts: bool = False):
        self._d = program[:]
        self._inputs = input_values
        self._output_halts = output_halts

        self._eop = False
        self._pc = pc
        self._m1 = 0
        self._m2 = 0
        self._m3 = 0
        self._result = -1

    # Decode functions
    def _digit(self, value: int, i: int) -> int:
        return (value // (10 ** i)) % 10

    def _decode(self):
        instruction = self._d[self._pc]

        self._op = instruction % 100
        self._m1 = self._digit(instruction, 2)
        self._m2 = self._digit(instruction, 3)
        self._m3 = self._digit(instruction, 4)

    # Memory access functions
    def _get(self, i: int, immediate_mode: int) -> int:
        value = self._d[self._pc + i]
        return value if immediate_mode else self._d[value]

    def _set(self, i: int, value: int):
        self._d[self._d[self._pc + i]] = value

    # Operation functions
    def _op_add(self):
        self._set(3, self._get(1, self._m1) + self._get(2, self._m2))
        self._pc += 4

    def _op_mul(self):
        self._set(3, self._get(1, self._m1) * self._get(2, self._m2))
        self._pc += 4

    def _op_in(self):
        self._set(1, self._inputs.pop(0))
        self._pc += 2

    def _op_out(self):
        self._result = self._get(1, 0)
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
        self._set(3, self._get(1, self._m1) < self._get(2, self._m2))
        self._pc += 4

    def _op_eq(self):
        self._set(3, self._get(1, self._m1) == self._get(2, self._m2))
        self._pc += 4

    def _op_eop(self):
        self._eop = True

    def _op_hcf(self):
        raise Exception(f"[PC:{self._pc}] Invalid operation '{self._op}'")

    # Exec functions
    def _exec(self):
        try:
            {
                1: self._op_add,
                2: self._op_mul,
                3: self._op_in,
                4: self._op_out,
                5: self._op_jit,
                6: self._op_jif,
                7: self._op_lt,
                8: self._op_eq,
                99: self._op_eop,
            }[self._op]()

        except IndexError:
            self._op_hcf()

    def run(self) -> int:
        if self._eop:
            return self._result

        while not self._eop:
            self._decode()
            self._exec()

        return self._result


def part1(program: List[int]) -> int:
    signal = 0

    for p in permutations(range(5)):
        amp = 0

        for phase in p:
            amp = Intcode(program, [phase, amp]).run()

        signal = max(signal, amp)

    return signal


def part2(program: List[int]) -> int:
    signal = 0

    for p in permutations(range(5, 10)):
        progs = []
        inputs = []
        for i in range(5):
            progs.append(program[:])
            inputs.append([p[i]])

        last = 0
        amp = 0
        pcs = [0] * 5
        while amp >= 0:
            for i in range(5):
                inputs[i].append(amp)

                m = Intcode(progs[i], inputs[i], pc=pcs[i], output_halts=True)

                amp = m.run()

                progs[i] = m._d
                pcs[i] = m._pc
                inputs[i] = m._inputs

            if amp >= 0:
                last = amp

        signal = max(signal, last)

    return signal


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        data = f.read()

    program = list(map(int, data.split(',')))

    print(f"Part 1: {part1(program)}")
    print(f"Part 2: {part2(program)}")
