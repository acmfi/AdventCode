import re
from copy import deepcopy
from collections import defaultdict

f = open('input')
data = [line for line in f.readlines()]

inputs = [line.strip('\n').split('-') for line in data]

caves_map = defaultdict(set)
for i,o in inputs:
    caves_map[i].add(o)
    caves_map[o].add(i)

big_cave_pattern = re.compile(r'^[A-Z]{1}')

def expand_paths(input,visited,path,small_visited):
    if input == 'end':
        complete_paths.add(path + '-end')
        return
    if input in caves_map.keys():
        visited.add(input)
        path += '-{}'.format(input) if len(path) > 0 else input
        for out in [x for x in caves_map[input] if x != 'start' and (x not in visited or (x in visited and not small_visited) or big_cave_pattern.match(x))]:
            if not big_cave_pattern.match(out) and out in visited and not small_visited:
                expand_paths(out,deepcopy(visited),path,True)
            else:
                expand_paths(out,deepcopy(visited),path,small_visited)

complete_paths = set()
expand_paths('start', set(),'',True)

print(len(complete_paths))

complete_paths = set()
expand_paths('start', set(),'',False)

print(len(complete_paths))