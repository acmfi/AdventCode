from procces import procces
import re

file = open('towerData', 'r')
# file = open('day7/rubenaguadoc/towerData', 'r')
inpt = file.read()
inpt2 = inpt

lefties = []
rigths = []
inpt = inpt.splitlines()
inpt2 = inpt2.splitlines()
inpt = [re.sub(r' \(.*\) -> ', ';', x) for x in inpt]
inpt = [re.sub(r' \(.*\)', '', x) for x in inpt]

for value in inpt:
    value = value.split(";")
    if(len(value) == 2):
        for prog in value[1].split(', '):
            lefties.append(prog)
    rigths.append(value[0])

for index, prog in enumerate(rigths):
    for prog2 in lefties:
        if(prog == prog2):
            rigths[index] = ''

for i in range(1, len(rigths), 1):
    rigths.remove('')

first = rigths[0]
print("Main proccess: " + first)

total = 0
ps = []
for i in inpt2:
    ps.append(procces(i))

for i in ps:
    if i.name == first:
        total = i.calculateWeigth(ps)

print("¡Éxito!")
