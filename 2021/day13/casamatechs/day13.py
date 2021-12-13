f = open('input')

input = [line.strip('\n') for line in f.readlines()]

matrix_input = [x.split(',') for x in input if ',' in x]
points = set()
for inp in matrix_input:
    points.add((int(inp[0]),int(inp[1])))
folds = [x.split(' ')[2].split('=') for x in input if 'fold' in x]

def fold_sheet(coord, axis, points, prnt):
    coord_del = set()
    coord_add = set()
    if coord == 'x':
        for x in points:
            if x[0] > axis:
                coord_del.add(x)
                coord_add.add((2*axis-x[0],x[1]))
    else:
        for y in points:
            if y[1] > axis:
                coord_del.add(y)
                coord_add.add((y[0],2*axis-y[1]))
    points -= coord_del
    points |= coord_add
    if prnt:
        print(len(points))

prnt = True
for coord,axis in folds:
    fold_sheet(coord,int(axis),points,prnt)
    prnt = False

x = []
y = []
for idx in points:
    x += [idx[0]]
    y += [idx[1]]

word = [[' '] * (max(x)+1) for _ in [[] for _ in range(max(y)+1)]]
for idx in range(len(x)):
    word[y[idx]][x[idx]] = '#'
for w in word:
        print(w)    