from sys import argv
from os import listdir

if len(argv) == 2 and argv[1] in listdir('./'):
    filename = argv[1]
elif 'input' in listdir('./'):
    filename = 'input'
else:
    print('Not a valid input file.')
    exit()


with open(filename, 'r') as f:
    colors = {}
    for l in f:
        l = l.strip('\n').strip('.')
        l = l.split(' contain ')
        key = l[0].replace(' bags', '')
        value = l[1].replace(' bags', '').replace(' bag', '')
        value = value.split(', ')
        value = {(i := v.split(' ', 1))[1] : int(i[0]) for v in value if v != 'no other'}
        colors[key] = value


def rec_star1(key, valid_colors):
    n = set()
    for c in colors:
        if key in colors[c]:
            valid_colors.add(c)
            n.add(c)
    for c in n:
        rec_star1(c, valid_colors)


def star1():
    valid_colors = set()
    rec_star1('shiny gold', valid_colors)
    print(len(valid_colors))


def rec_star2(key):
    if not colors[key]:
        return 0
    else:
        total = 0
        for bag in colors[key]:
            total += colors[key][bag] + colors[key][bag] * rec_star2(bag)
        return total


def star2():
    print(rec_star2('shiny gold'))


star1()
star2()


    


