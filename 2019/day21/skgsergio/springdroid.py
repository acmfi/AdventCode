#!/usr/bin/env python3
import sys

from typing import List, Optional


DEBUG = False


class Intcode:
    def __init__(self, program: List[int],
                 input_values: Optional[List[int]] = None):
        self._d = program[:]

        self._inputs = input_values if input_values else []

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

    def run(self, input_values: Optional[List[int]] = None) -> Optional[int]:
        if self._eop:
            raise Exception(f"[EOP] Machine already in End Of Program state.")

        if input_values is not None:
            self._inputs = input_values

        self._output = None
        self._halt = False

        while not self._eop and self._output is None:
            self._decode()
            self._exec()

        return self._output


def run(data: List[int], script: List[str]) -> int:
    m = Intcode(data)
    out = 0

    while out != 10:
        out = m.run()

        if m._eop or out is None:
            break

        print(chr(out), end="")

    print("\n".join(script))

    out = m.run([ord(c) for c in "\n".join(script) + "\n"])

    while not m._eop:
        out = m.run()

        if out > 255:
            break

        print(chr(out), end="")

    return out


def part1(data: List[int]) -> int:
    script = (
        "NOT A T",
        "OR T J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "WALK"
    )

    return run(data, script)


def part2(data: List[int]) -> int:
    script = (
        "NOT A T",
        "OR T J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "NOT H T",
        "NOT T T",
        "OR E T",
        "AND T J",
        "RUN",
    )

    return run(data, script)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input>", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        data = f.read()

    program = list(map(int, data.split(',')))

    print(f"Part 1: {part1(program)}")
    print(f"Part 2: {part2(program)}")
