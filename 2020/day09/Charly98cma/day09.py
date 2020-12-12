 #!/usr/bin/python
import sys

preambule = 25
limits = [0, preambule]
l = [int(x) for x in open(sys.argv[1],'r').read().split('\n')[:-1]]

for num in l[preambule:]:
    found = False
    window = l[limits[0]:limits[1]]
    for el in window:
        if (num-el) in window and (num-el) != el:
            found = True
    if not found:
        print("1st STAR SOLUTION ->", num)
        break
    limits = [x+1 for x in limits]
