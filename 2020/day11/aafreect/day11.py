from sys import argv, exit
from os import listdir

if len(argv) == 2 and argv[1] in listdir('./'):
    filename = argv[1]
elif 'input' in listdir('./'):
    filename = 'input'
else:
    print("Not a valid input file.")
    exit()

with open(filename, 'r') as f:
    seatmatrix = []
    values = {'.':-1, 'L':0, '#':1}
    for l in f:
        seatmatrix.append([values[c] for c in l])

def surrounding_seats(seat, matrix):
    s = ma
    return sum([])

def step(matrix)
    copy = [row.copy() for row in matrix]
    for row in len(matrix):
        for seat in len(matrix[row]):
            ] 
