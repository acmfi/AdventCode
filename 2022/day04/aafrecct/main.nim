from std/strutils import parse_int, split
from std/sugar import collect

proc `in`(a, b: seq[int]): bool =
  (a[0] >= b[0]) and (a[1] <= b[1])

proc overlap(a, b: seq[int]): bool =
  (a[0] <= b[0] and b[0] <= a[1]) or
  (a[0] <= b[1] and b[1] <= a[1]) or
  (b[0] <= a[0] and a[0] <= b[1]) or
  (b[0] <= a[1] and a[1] <= b[1])

proc main(): array[0..1, int] =
  result = [0, 0]
  for line in lines "input":
    let
      tasks = line.split(",")
      task1 = collect:
        for task in tasks[0].split("-"): parse_int(task)
      task2 = collect:
        for task in tasks[1].split("-"): parse_int(task)
    result[0] +=  int(task1 in task2 or task2 in task1)
    result[1] +=  int(overlap(task1, task2))

echo main()
