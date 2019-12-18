#!/usr/bin/env python3
import sys

from typing import List, Dict, Tuple, Optional


DEBUG = False

TILECHR = {
    0: ' ',
    1: '█',
    2: '⊞',
    3: '⟺',
    4: '•'
}


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


def part1(data: List[int]) -> int:
    m = Intcode(data)

    outputs = []
    while True:
        out = m.run([])
        if m._eop:
            break

        outputs.append(out)

    return outputs[2::3].count(2)


def draw_screen(screen: Dict[Tuple[int, int], int], score):
    frame = '\033[2J\033[1;1H'

    for y in range(max(map(lambda x: x[1], screen.keys())) + 1):
        for x in range(max(map(lambda x: x[0], screen.keys())) + 1):
            frame += TILECHR[screen.get((x, y), 0)]
        frame += "\n"

    print(f"{frame}{score}")


def part2(data: List[int], print_screen: bool) -> int:
    m = Intcode(data)
    m._d[0] = 2  # Using Cheat Engine to hack credits

    tiles = {}

    c = 0
    x, y, z = 0, 0, 0
    ball = 0
    paddle = 0
    joystick = 0
    score = 0
    while True:
        out = m.run([joystick])
        if m._eop:
            break

        if c % 3 == 0:
            x = out

        elif c % 3 == 1:
            y = out

        else:
            z = out

            tiles[(x, y)] = z

            if z == 4:
                ball = x

            elif z == 3:
                paddle = x

            if (x, y) == (-1, 0) and z not in range(0, 5):
                score = z

            if ball < paddle:
                joystick = -1

            elif ball > paddle:
                joystick = 1

            else:
                joystick = 0

            if print_screen:
                draw_screen(tiles, score)

        c += 1

    return score


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(f"Usage: {sys.argv[0]} <input> [print]", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        data = f.read()

    program = list(map(int, data.split(',')))

    p1 = part1(program)
    p2 = part2(program, len(sys.argv) == 3 and sys.argv[2] == "print")

    print(f"Part 1: {p1}")
    print(f"Part 2: {p2}")
