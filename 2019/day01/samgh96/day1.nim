import parseutils

let file = "input.txt"
let part = 2

proc calc_fuel(n: int): int =
  return int(n / 3) - 2

proc calc_all_fuels(n: int): int =
  var rec_n = calc_fuel(n)
  var carry = 0
  while rec_n > 0:
    carry += rec_n
    rec_n = calc_fuel(rec_n)
  return carry

proc get_answer(filename: string): int =
  let input = open(filename)
  var acc = 0

  for line in input.lines:
    var parsedInt = 0
    discard parseInt(line, parsedInt)
    if part == 1:
      acc += calc_fuel(parsedInt)
    elif part == 2:
      acc += calc_all_fuels(parsedInt)
  return acc

echo get_answer(file)
