f = open('input')

input = [line.strip('\n') for line in f.readlines()]

X=[-1,-1,-1,0,0,1,1,1]
Y=[-1,0,1,-1,1,-1,0,1]

grid = [] # This implemetation sucks and should be changed to a key-value data structure but :)
for line in input:
    grid.append([c for c in line])

def check_adj(seat_x,seat_y):
    for i in range(len(X)):
        if 0<=seat_x+X[i]<len(grid) and 0<=seat_y+Y[i]<len(grid[0]) and grid[seat_x+X[i]][seat_y+Y[i]] == '#':
            return False
    return True

def check_ocup(seat_x,seat_y):
    occup = 0
    for i in range(len(X)):
        if 0<=seat_x+X[i]<len(grid) and 0<=seat_y+Y[i]<len(grid[0]) and grid[seat_x+X[i]][seat_y+Y[i]] == '#':
            occup += 1
    return occup

changes = True
while changes:
    changes = False
    coords_to_occupied = []
    coords_to_empty = []
    for idx,row in enumerate(grid):
        for idy,seat in enumerate(row):
            if seat == 'L' and check_adj(idx,idy):
                coords_to_occupied.append((idx,idy))
                changes = True
            elif seat == '#' and check_ocup(idx,idy) >=4:
                coords_to_empty.append((idx,idy))
                changes = True
    for cocp in coords_to_occupied:
        grid[cocp[0]][cocp[1]] = '#'
    for cemp in coords_to_empty:
        grid[cemp[0]][cemp[1]] = 'L'

print([x for y in grid for x in y].count('#'))

# Star 2

grid = []
for line in input:
    grid.append([c for c in line])

def check_adj(seat_x,seat_y):
    for i in range(len(X)):
        xi = X[i]
        yi = Y[i]
        while 0<=seat_x+xi<len(grid) and 0<=seat_y+yi<len(grid[0]):
            if grid[seat_x+xi][seat_y+yi] == '#':
                return False
            elif grid[seat_x+xi][seat_y+yi] == 'L':
                break
            xi += X[i]
            yi += Y[i]
    return True

def check_ocup(seat_x,seat_y):
    occup = 0
    for i in range(len(X)):
        xi = X[i]
        yi = Y[i]
        while 0<=seat_x+xi<len(grid) and 0<=seat_y+yi<len(grid[0]):
            if grid[seat_x+xi][seat_y+yi] == '#':
                occup += 1
                break
            elif grid[seat_x+xi][seat_y+yi] == 'L':
                break
            xi += X[i]
            yi += Y[i]
    return occup

changes = True
while changes:
    changes = False
    coords_to_occupied = []
    coords_to_empty = []
    for idx,row in enumerate(grid):
        for idy,seat in enumerate(row):
            if seat == 'L' and check_adj(idx,idy):
                coords_to_occupied.append((idx,idy))
                changes = True
            elif seat == '#' and check_ocup(idx,idy) >=5:
                coords_to_empty.append((idx,idy))
                changes = True
    for cocp in coords_to_occupied:
        grid[cocp[0]][cocp[1]] = '#'
    for cemp in coords_to_empty:
        grid[cemp[0]][cemp[1]] = 'L'

print([x for y in grid for x in y].count('#'))