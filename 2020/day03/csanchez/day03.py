f = open('input.txt')

puzzle = f.read().splitlines()

trees_counter = 0
row = 1
column = 3

# First star
while row < len(puzzle):
    if puzzle[row][column] == '#':
        trees_counter += 1
    row += 1
    column += 3
    if column >= len(puzzle[0]):
        column -= len(puzzle[0])

print('1st Star solution: ', trees_counter)

# Second star

row_slopes = [1, 1, 1, 2]
column_slopes = [1, 5, 7, 1]
temp_trees_counter = 0

for idx, val in enumerate(row_slopes):
    row = row_slopes[idx]
    column = column_slopes[idx]
    while row < len(puzzle):
        if puzzle[row][column] == '#':
            temp_trees_counter += 1
        row += row_slopes[idx]
        column += column_slopes[idx]
        if column >= len(puzzle[0]):
            column -= len(puzzle[0])
    trees_counter *= temp_trees_counter
    temp_trees_counter = 0

print('2nd Star solution: ', trees_counter)