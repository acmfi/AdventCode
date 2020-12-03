with open("input.txt", "r") as f:
    f.readline() # Dump the first line
    treeCounter = 0
    row = 3
    for l in f:
        line = list(l.strip())
        if line[row] == '#':
            treeCounter += 1
        row += 3
        row %= len(line)
    print("1st STAR SOLUTION =", treeCounter)
