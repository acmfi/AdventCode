import sys
from itertools import cycle

# Star 2

visited = set([0])
freq = 0

for n in cycle(map(int, sys.stdin.readlines())):
    freq += n
    if freq in visited:
        print ("Star2: ", freq)
        break
    visited.add(freq)
