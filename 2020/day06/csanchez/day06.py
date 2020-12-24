import functools
from collections import Counter

f = open('input.txt')

groups = [group.split('\n') for group in f.read().split('\n\n')]

yes_cells = 0

for group in groups:
    cells = list(dict.fromkeys(functools.reduce(lambda a,b: a+b, group)))
    yes_cells += len(cells)

print('First star solution: ', yes_cells)

everyone_yes_cells = 0

for group in groups:
    n_group = len(group)
    counter = Counter(list(functools.reduce(lambda a,b: a+b, group)))
    everyone_yes_cells += len(list(filter(lambda n: n == n_group, counter.values())))

print('Second star solution: ', everyone_yes_cells)