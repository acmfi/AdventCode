#!/usr/bin/env python3

import sys

from collections import defaultdict

from queue import Queue, Empty
from multiprocessing import pool


def solve(ins, pid=0, qin=None, qout=None):
    r = defaultdict(int)
    r['p'] = pid

    def rorint(v):
        return r[v] if v in r.keys() else int(v)

    i = 0
    c = 0
    s = None

    while 0 <= i < len(ins):
        cmd = ins[i].split()

        if cmd[0] == 'set':
            r[cmd[1]] = rorint(cmd[2])

        elif cmd[0] == 'add':
            r[cmd[1]] += rorint(cmd[2])

        elif cmd[0] == 'mul':
            r[cmd[1]] *= rorint(cmd[2])

        elif cmd[0] == 'mod':
            r[cmd[1]] %= rorint(cmd[2])

        elif cmd[0] == 'snd':
            c += 1
            s = rorint(cmd[1])

            if qout:
                qout.put(s)

        elif cmd[0] == 'rcv':
            if qin:
                try:
                    r[cmd[1]] = qin.get(timeout=1)

                except Empty:
                    return c

            elif r[cmd[1]] != 0:
                return s

        elif cmd[0] == 'jgz':
            if rorint(cmd[1]) > 0:
                i += rorint(cmd[2])
                continue

        i += 1

    return c


def part_1(ins):
    return solve(ins)


def part_2(ins):
    p = pool.ThreadPool(processes=2)
    q1, q2 = Queue(), Queue()

    p1 = p.apply_async(solve, (ins, 0, q1, q2))
    p2 = p.apply_async(solve, (ins, 1, q2, q1))

    return p1.get(), p2.get()


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage: {} <input>".format(sys.argv[0]),
              file=sys.stderr)
        exit(1)

    ins = None
    with open(sys.argv[1], 'r') as f:
        ins = f.readlines()

    print("Part 1: {}".format(part_1(ins)))
    print("Part 2: {}".format(part_2(ins)[1]))
