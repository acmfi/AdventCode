from std/strutils import parseInt
from std/math import sum

proc push[I, T](arr: ref array[I, T], i: I, elem: T) =
  if i < arr[].len - 1:
    push(arr, i + 1, arr[i])
  arr[i] = elem

proc main(): ref array[0..2, int] =
  var elf_total: int = 0
  result = new array[0..2, int]
  result[] = [0, 0, 0]
  for line in lines "input":
    if line.len > 0:
      elf_total += parseInt(line)
    else:
      for i, elf in result[].pairs():
        if elf_total > elf:
          result.push(i, elf_total)
          break
      elf_total = 0
  return result

let solution = main()
echo solution[], " ", sum(solution[])

