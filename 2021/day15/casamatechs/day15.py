from collections import defaultdict
import math

f = open('input')

input = [line.strip('\n') for line in f.readlines()]

nodes = defaultdict(int)
paths = defaultdict(list)
x_mov = [0,1,0,-1]
y_mov = [1,0,-1,0]
for idx, row in enumerate(input):
    for idy, column in enumerate(row):
        nodes[(idx,idy)] = int(column)
        for i in range(4):
            xx = idx + x_mov[i]
            yy = idy + y_mov[i]
            if 0<=xx<len(row) and 0<=yy<len(row):
                paths[(idx,idy)] += [(xx,yy)]

def dijkstra(nodes,size,debug=False):
    unvisited_nodes = defaultdict(lambda:math.inf)
    visited_nodes = set()
    unvisited_nodes[(0,0)] = 0
    current = (0,0)
    it = 0
    while True:
        for neigh in paths[current]:
            if nodes[neigh] + unvisited_nodes[current] < unvisited_nodes[neigh]:
                unvisited_nodes[neigh] = unvisited_nodes[current] + nodes[neigh]
        visited_nodes.add(current)           
        if debug and it % 1000 == 0:
            print('Size unvisited nodes:',len(unvisited_nodes))
        unvisited_nodes = defaultdict(lambda:math.inf,{x:y for x,y in unvisited_nodes.items() if x not in visited_nodes})
        current = min(unvisited_nodes,key=unvisited_nodes.get)
        it += 1
        if current == (size-1,size-1):
            break
    return unvisited_nodes[(size-1,size-1)]
print(dijkstra(nodes,len(input)))

expand_idx = [[(0,0)],
    [(1,0),(0,1)],
    [(0, 2), (1, 1), (2, 0)],
    [(0, 3), (1, 2), (2, 1), (3, 0)],
    [(0, 4), (1, 3), (2, 2), (3, 1), (4, 0)],
    [(4, 1), (3, 2), (2, 3), (1, 4)],
    [(4, 2), (3, 3), (2, 4)],
    [(4, 3), (3, 4)],
    [(4, 4)]] # I know there are ways to do this in a loop, but I'm just tired and my brain is not working anymore.
def expand_nodes(size,debug=False):
    original_keys = list(nodes.keys())
    for x,y in original_keys:
        for val,exp in enumerate(expand_idx):
            for xx,yy in exp:
                nx = len(input)*xx+x
                ny = len(input)*yy+y
                nodes[(nx,ny)] = (nodes[(x,y)] + val) % 9 if (nodes[(x,y)] + val) % 9 != 0 else 9
                paths[(nx,ny)] = []
                for i in range(4):
                    nxx = nx + x_mov[i]
                    nyy = ny + y_mov[i]
                    if 0<=nxx<size and 0<=nyy<size:
                        paths[(nx,ny)] += [(nxx,nyy)]
    if debug:
        for x in range(size):
            line = ''
            for y in range(size):
                line += str(nodes[(x,y)])
            print(line)
expand_nodes(len(input)*5)
print(dijkstra(nodes,len(input)*5))