import strutils
import sequtils
import random

let file = "input.txt"
let part = 2

type
  Opcode = tuple[ins: int, pos: int]

proc exec_program(program: seq[int]): seq[int] =
  var state: seq[int] = program
  var opcode: Opcode = (ins: 0, pos: 0)
  
  while opcode.ins != 99:
    opcode.ins = state[opcode.pos]
    echo opcode
    case opcode.ins:
      of 1: # add
        echo state[state[opcode.pos + 1]], " + ", state[state[opcode.pos + 2]]
        state[state[opcode.pos + 3]] = state[state[opcode.pos + 1]] + state[state[opcode.pos + 2]]
        echo  " = ", state[opcode.pos + 3]
        opcode.pos += 4
      of 2: # mul
        echo state[state[opcode.pos + 1]], " * ", state[state[opcode.pos + 2]]
        state[state[opcode.pos + 3]] = state[state[opcode.pos + 1]] * state[state[opcode.pos + 2]]
        echo " = ", state[opcode.pos + 3]
        opcode.pos += 4
      of 99:
        return state
      else:
        echo "patata"

proc get_answer(filename: string): int =
  let input = open(filename)
  
  var program = input.readline().split(',').map do (x: string) -> int: parseInt(x)
  if part == 1:
    program[1] = 12
    program[2] = 2
  if part == 2:
    var ret = 0
    while ret != 19690720:
      program[1] = rand(0..99)
      program[2] = rand(0..99)
      ret = exec_program(program)[0]
    echo program[1], ", ", program[2]
    return 100 * program[1] + program[2]
  return exec_program(program)[0]

echo get_answer(file)
