def star1():
    with open("input.txt", "r") as f:
        length = len(f.readline().strip()) # Dump the first line
        forest = []
        treeCounter = 0
        row = 0
        col = 3
        for l in f:
            forest.append(list(l.strip()))
            if forest[row][col] == '#':
                treeCounter += 1
            col = (col + 3) % length
            row += 1
        print("1st STAR SOLUTION =", treeCounter)


star1()
