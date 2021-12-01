from sys import argv
from os import listdir

if len(argv) == 2 and argv[1] in listdir('./'):
    filename = argv[1]
elif 'input' in listdir('./'):
    filename = 'input'
else:
    print('Not a valid input file.')
    exit()


def lines(skip):
    with open(filename, 'r') as f:
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
