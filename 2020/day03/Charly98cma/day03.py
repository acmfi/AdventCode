with open("input.txt", "r") as f:
    # List of lists (tree lines)
    forest = []
    # [[col, row]]
    slopes = [[1,1],[3,1],[5,1],[7,1],[1,2]]
    trees =   [[0],  [0],  [0],  [0],  [0]]
    for l in f:
        forest.append(list(l.strip()))
    # Length of each tree line
    length = len(l.strip())
    # Loop through tree lines
    for x in range(len(forest)):
        # Loop through each slope
        for y in range(len(slopes)):
            try:
                if forest[slopes[y][1]*x][(slopes[y][0]*x)%length] == '#':
                    trees[y][0] += 1
            except IndexError:
                # Exception used to mitigate the error
                # introduced by the last slope
                pass
    res = 1
    for x in trees:
        res *= x[0]
    print("1st STAR SOLUTION =", trees[1][0])
    print("2nd STAR SOLUTION =", res)
