import std/sets
from std/sequtils import map

const itemtypes = "0abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

proc any[T](someSet: SomeSet[T]): T =
  for item in someSet:
    return item

proc uniqueStringIntersection(strings: varargs[string]): char =
  var string_sets: seq[HashSet[char]]
  for str in strings:
    string_sets.add(toHashSet(str))
  var intersection = string_sets[0] * string_sets[1]
  for i in 2 ..< string_sets.len:
    intersection = intersection * string_sets[i]
  echo intersection
  result = any intersection

proc main(): array[0..1, int] =
  result = [0, 0]
  var 
    triple: array[0..2, string]
    counter = 0
  for line in lines "input":
    echo line
    let
      first_container = line[0 .. len(line) div 2 - 1]
      second_container = line[len(line) div 2 .. ^1]
      common_letter = uniqueStringIntersection(first_container, second_container)
    result[0] +=  itemtypes.find(common_letter)
    triple[counter] = line
    counter += 1
    if counter == 3:
      counter = 0
      let badge = uniqueStringIntersection(triple[0], triple[1], triple[2])
      result[1] += itemtypes.find(badge) 

let solution = main()
echo solution
