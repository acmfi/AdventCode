 #!/usr/bin/python
import sys

preambule = 25
limits = [0, preambule]
l = [int(x) for x in open(sys.argv[1],'r').read().split('\n')[:-1]]

# 1st STAR SOLUTION
for num in l[preambule:]:
    found = False
    # Sliding window
    window = l[limits[0]:limits[1]]
    for el in window:
        # If the rest is in the sliding window,
        # the num is not the solution
        if (num-el) in window and (num-el) != el:
            found = True
            break
    # If no num has been found that is the sum of two previous,
    # that number is the solution
    if not found:
        print("1st STAR SOLUTION ->", num)
        break
    # Move the window 1 position
    limits = [x+1 for x in limits]

# 2nd STAR SOLUTION
for x in l:
    found = False
    # Create a list with the numbers we are adding
    window = [x]
    # Check the following numbers
    for y in l[l.index(x)+1:]:
        window.append(y)
        total = sum(window)
        # Check the sum of the numbers on the list
        if total > num:
            break
        elif total == num:
            found = True
            break
    if found:
        print("2nd STAR SOLUTION ->", min(window)+max(window))
        break
