from sys import argv
from os import listdir
from math import prod

if len(argv) == 2 and argv[1] in listdir('./'):
    filename = argv[1]
elif 'input' in listdir('./'):
    filename = 'input'
else:
    print('Not a valid input file.')
    exit()


with open(filename, 'r') as f:
    adapters = [int(i) for i in f.readlines()]
    adapters.sort()

last = 0
t = 0
m = []
threes = 1
ones = 0
for i in adapters:
    print(i)
    if i - last == 3:
        threes += 1
        if t > 1:
            m.append(t)
        t = 0
    elif i - last == 1:
        t += 1
        ones += 1
    last = i
if t > 1:
    m.append(t)

print(ones, threes)

print(ones * threes)

print(m)
m = [2**(i-1) if i != 4 else 2**(i-1)-1 for i in m]
print(m)
print(prod(m))


