def lines(skip):
    with open('day03.input', 'r') as f:
        while (l := f.readline()) != '':
            for i in range(skip):
                f.readline()
            l = l.replace('\n', '')
            yield l


slopes = [(1, 1),
          (3, 1),
          (5, 1),
          (7, 1),
          (1, 2)]


def trees(slope):
    length = len(next(lines(0)))
    trees = 0
    for i, line in enumerate(lines(slope[1] - 1)):
        pos = (slope[0] * i) % length
        trees += 1 if line[pos] == '#' else 0
    return trees


mul = 1
for slope in slopes:
    t = trees(slope)
    mul *= t
    print(slope, t)

print(mul)
