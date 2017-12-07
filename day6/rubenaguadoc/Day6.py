import re

inpt = "2	8	8	5	4	2	3	1	5	5	1	2	15	13	5	14"
inpt = list(map(int, re.split(r'\t+', inpt)))
states = [list(inpt)]
seen = False
count = 0

while(not seen):
    bigest = max(inpt)
    indx = inpt.index(bigest)
    inpt[indx] = 0
    for i in range(1, bigest + 1, 1):
        stack = i + indx
        while(stack >= len(inpt)):
            stack -= len(inpt)
        inpt[stack] += 1
    for state in states:
        if(list(inpt) == state):
            seen = True
    states.append(list(inpt))
    count += 1

print(count)

for index, state in enumerate(states):
    for index2, state2 in enumerate(states):
        if(state == state2 and not index == index2):
            print(index2 - index)
