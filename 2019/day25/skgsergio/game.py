#!/usr/bin/env python3
import sys
import random

from itertools import combinations

from typing import List, Tuple, Optional


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


def run(data: List[int]):
    m = Intcode(data)

    last_line = ""
    while True:
        o = m.run()

        if m._eop or o is None:
            break

        last_line += chr(o)
        print(chr(o), end='')

        if last_line[-1] == "\n":
            if last_line == "Command?\n":
                command = input("> ")

                m._inputs = [ord(c) for c in command] + [ord("\n")]

            last_line = ""


def brute(data: List[int], exclude_items: List[str]):
    opposite_door = {
        "north": "south",
        "south": "north",
        "east": "west",
        "west": "east",
        "init": "init"
    }

    last_line = ""
    last_door = "init"
    last_inv: List[str] = []

    current_inv: List[str] = []
    current_inv_refresh = True

    eq_inv_count = 0
    eq_inv_max = 25

    comb_len = 1
    combs: List[Tuple[str, ...]] = []
    current_comb: Tuple[str, ...] = ()

    doors: List[str] = []
    items: List[str] = []

    reading_doors = False
    reading_items = False
    reading_inv = False

    first_drop = True

    checkpoint = False

    pending_cmds: List[str] = []

    m = Intcode(data)
    while True:
        o = m.run()

        if m._eop or o is None:
            break

        last_line += chr(o)
        print(chr(o), end='')

        if last_line[-1] == "\n":
            if reading_doors:
                if not last_line.strip():
                    reading_doors = False

                else:
                    doors.append(last_line[2:-1])

            elif reading_items:
                if not last_line.strip():
                    reading_items = False

                else:
                    items.append(last_line[2:-1])

            elif reading_inv:
                if not last_line.strip():
                    reading_inv = False
                    current_inv_refresh = False

                else:
                    current_inv.append(last_line[2:-1])

            elif last_line[:2] == "==":
                checkpoint = last_line == "== Security Checkpoint ==\n"

            elif last_line == "Doors here lead:\n":
                doors = []
                reading_doors = True

            elif last_line == "Items here:\n":
                items = []
                reading_items = True

            elif last_line == "Items in your inventory:\n":
                reading_inv = True
                last_inv = current_inv.copy()
                current_inv = []

            elif last_line == "Command?\n" and checkpoint:
                if current_inv_refresh:
                    command = "inv"

                elif eq_inv_count >= eq_inv_max:
                    if first_drop:
                        first_drop = False
                        pending_cmds = [f"drop {i}" for i in current_inv]

                    elif not combs and not pending_cmds:
                        comb_len += 1
                        assert comb_len < len(current_inv)
                        combs = list(combinations(current_inv, comb_len))

                    if combs and not pending_cmds:
                        pending_cmds = [f"drop {i}" for i in current_comb]
                        current_comb = combs.pop(0)
                        pending_cmds += [f"take {i}" for i in current_comb]
                        pending_cmds += set(doors) - set([opposite_door[last_door]])

                    command = pending_cmds.pop(0)

                else:
                    current_inv_refresh = True
                    eq_inv_count += current_inv == last_inv

                    command = last_door = opposite_door[last_door]

                print(f"> {command}")
                m._inputs = [ord(c) for c in command] + [ord("\n")]

            elif last_line == "Command?\n" and not checkpoint:
                command = ""

                if items:
                    while items:
                        item = items.pop(0)

                        if item not in exclude_items:
                            command = f"take {item}"
                            break

                elif not command and doors:
                    door = random.choice(doors)

                    if last_door and len(doors) > 1:
                        while door == opposite_door[last_door]:
                            door = random.choice(doors)

                    command = last_door = door

                print(f"> {command}")
                m._inputs = [ord(c) for c in command] + [ord("\n")]

            last_line = ""


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} play/brute <input> [exclude]", file=sys.stderr)
        exit(1)

    with open(sys.argv[2], 'r') as f:
        data = f.read()

    program = list(map(int, data.split(',')))

    if sys.argv[1] == "play":
        run(program)

    elif sys.argv[1] == "brute":
        with open(sys.argv[3], 'r') as f:
            exclude = f.read().splitlines()

        brute(program, exclude)
