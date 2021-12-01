from sys import argv
from os import listdir

if len(argv) == 2 and argv[1] in listdir('./'):
    filename = argv[1]
elif 'input' in listdir('./'):
    filename = 'input'
else:
    print('Not a valid input file.')
    exit()


mod_classes = {i:[] for i in range(10)}


def next_number():
    with open(filename, 'r') as f:
        while (l := f.readline()) != '':
            yield int(l)


def star1():
    for i in next_number():
        mod_i = i % 10
        for j in mod_classes[(10-mod_i) % 10]:
            if (i + j == 2020):
                print('STAR 1 numbers are: ', i, 'and', j)
                print('Solution:', i * j)
                return
        mod_classes[mod_i].append(i)


def star2():
   for i in next_number():
       for j in next_number():
           if j < 2020 - i:
               for k in next_number():
                   if i + j + k == 2020:
                       print ('STAR 2 numbers are: ', i, ',', j, 'and', k)
                       print ('Solution:', i * j * k)
                       return

star1()
star2()
