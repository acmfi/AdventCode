import std/lists
import std/re
import std/strutils
from std/sugar import collect


type
  Tower = DoublyLinkedList[char]
  Bay = seq[Tower]
  Instruction = array[0..2, int]

proc reverse (tower: Tower): Tower =
  var t = initDoublyLinkedList[char]()
  var tail = tower.tail
  while tail != nil:
    t.add(tail.value)
    tail = tail.prev
  return t

proc parse (towers: var Bay, instructions: var seq[Instruction]) =
  var parse_instructions = false
  let instruction_re = re"move (\d+) from (\d+) to (\d+)"
  for line in lines "input":
    if line == "":
      parse_instructions = true
    elif not parse_instructions:
      var tower_i = 0
      var space_counter = 0
      for c in line:
        case c:
          of '[':
            while towers.len <= tower_i: towers.add(initDoublyLinkedList[char]())
            space_counter = 0
          of 'A'..'Z':
            towers[tower_i].add(c)
          of ']':
            inc tower_i
          of ' ':
            inc space_counter
            if space_counter == 4:
              space_counter = 0
              inc tower_i
          else:
            discard
    else:
      var matches: array[0..2, string]
      let valid_instruction = match(line, instruction_re, matches)
      if valid_instruction:
        let instruction: Instruction = [matches[0].parseInt, matches[1].parseInt, matches[2].parseInt]
        instructions.add(instruction)
  for i, tower in towers:
    towers[i] = tower.reverse

proc main() =
  var towers: Bay
  var towers_copy: Bay
  var cargo = initDoublyLinkedList[char]()
  var instructions: seq[Instruction]
  parse(towers, instructions)
  echo "Towers: "
  for tower in towers:
    towers_copy.add(tower.copy)
    echo tower
  echo "--------------------------------"

  for instruction in instructions:
    let (ammount, from_tower, to_tower) = (instruction[0], instruction[1], instruction[2])
    for i in 0 ..< ammount:
      towers[to_tower - 1].add(towers[from_tower - 1].tail.value)
      towers[from_tower - 1].remove(towers[from_tower - 1].tail)
      cargo.add(towers_copy[from_tower - 1].tail.value)
      towers_copy[from_tower - 1].remove(towers_copy[from_tower - 1].tail)
    for i in 0 ..< ammount:
      towers_copy[to_tower - 1].add(cargo.tail.value)
      cargo.remove(cargo.tail)
  
  for tower in towers:
    echo tower 
  echo "--------------------------------"
  for tower in towers_copy:
    echo tower
  let star1 = collect(for tower in towers: tower.tail.value).join
  let star2 = collect(for tower in towers_copy: tower.tail.value).join
  echo "--------------------------------"
  echo "Star 1: ", star1
  echo "Star 2: ", star2
main()
