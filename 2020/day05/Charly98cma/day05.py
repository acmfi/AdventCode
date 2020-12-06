#!/usr/bin/python
import sys

with open(sys.argv[1], 'r') as myFile:
    passes = []
    maxID = 0
    minID = sys.maxsize
    for l in myFile:
        ID = int(l.replace('F','0').replace('L','0').replace('B','1').replace('R','1'), 2)
        passes.append(ID)
        maxID = max(maxID, ID)
        minID = min(minID, ID)
    passes.sort()
    print("1st STAR SOLUTION ->", maxID)
    print("2nd STAR SOLUTION ->", set(list(range(minID,maxID))) - set(passes))
