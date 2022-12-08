proc all_different_chars(str: string): bool =
  var count = 0
  for c in str:
    for c2 in str:
      count += int(c == c2)
  return count == len(str)

proc first_marker(str: string, length: int): int =
  for i in 0..str.len - length:
    echo str[i..i + length - 1]
    if all_different_chars(str[i..i + length - 1]):
      return i + length
  return -1

proc main(): array[0..1, int] =
  result = [0, 0]
  for line in lines "input":
    result[0] = first_marker(line, 4)
    result[1] = first_marker(line, 14)
    return result

echo main()
