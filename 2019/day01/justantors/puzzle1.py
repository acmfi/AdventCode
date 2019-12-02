#!/usr/bin/env python3
import sys

def calculatePart2(list):
    result = 0
    for mod in list:
        while((mod := int(mod/3)-2) > 0):
            result += mod
    return result

if(len(sys.argv) != 2):
    print('python3 puzzle1.py <input_file.txt>')
    exit(1)

input = open(sys.argv[1], 'r') # Open file
input_formatted = list(map(lambda str : int(str), input.readlines())) # Cast every element (str) to an int
result1 = list(map(lambda num: int(num/3) - 2, input_formatted))
result2 = calculatePart2(input_formatted)
print('Puzzle 1: {}'.format(sum(result1)))
print('Puzzle 2: {}'.format(result2))
