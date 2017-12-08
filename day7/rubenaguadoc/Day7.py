import re

file = open('towerData', 'r')
inpt = file.read()

lefties = []
rigths = []
inpt = inpt.splitlines()
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

print(rigths[0])
