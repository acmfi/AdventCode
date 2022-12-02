type Shape = enum
  Rock = 1, Paper = 2, Scissors = 3

proc `%`(i, j: int): int =
  result = i mod j
  if result < 0: result += j


proc vs(a, b: Shape): int =
  if a == b: 3
  elif a == Shape.Rock and b == Shape.Scissors: 0
  elif a == Shape.Paper and b == Shape.Rock: 0
  elif a == Shape.Scissors and b == Shape.Paper: 0
  else: 6

proc counter(a: Shape, r: int): int =
  (r div 3 - 1 + int(a) - 1) % 3 + 1

proc s1_round(a, b: Shape): int =
  int(b) + vs(a, b)

proc s2_round(a: Shape, r: int): int =
  counter(a, r) + r

proc parseShape(s: string): Shape =
  case s
  of "A", "X": Rock
  of "B", "Y": Paper
  of "C", "Z": Scissors
  else: raise newException(ValueError, "Invalid shape: " & s)

proc parseResult(s: string): int =
  case s
  of "X": 0
  of "Y": 3
  of "Z": 6
  else: raise newException(ValueError, "Invalid result: " & s)

proc main(): array[0..1, int] =
  result = [0, 0]
  for line in lines "input":
    let
      a = parseShape line[0 .. 0]
      b = parseShape line[2 .. 2]
      c = parseResult line[2 .. 2]
    result[0] += s1_round(a, b)
    result[1] += s2_round(a, c)

let solution = main()
echo solution

