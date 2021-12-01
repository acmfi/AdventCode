from sys import argv
from os import listdir

if len(argv) == 2 and argv[1] in listdir('./'):
    filename = argv[1]
elif 'input' in listdir('./'):
    filename = 'input'
else:
    print('Not a valid input file.')
    exit()


def groups(star):
    with open(filename, 'r') as f:
        s = set()
        i = 0
        for line in f:
            line = line.strip('\n')
            if line:
                print(line)
                if not (star - 1 and i):
                    s = s.union({q for q in line})
                else:
                    s = s.intersection({q for q in line})
                i += 1
            else:
                yield s
                i = 0
                s.clear()
        else:
            yield s


def star1():
    total = 0
    for group in groups(1):
        total += len(group)
    print(total)


def star2():
    total = 0
    for group in groups(2):
        total += len(group)
    print(total)


star1()
star2()
