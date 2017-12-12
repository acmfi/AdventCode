# file = open('stream', 'r')
file = open('day9/rubenaguadoc/stream', 'r')
inpt = file.read()

garbageCounter = 0
importantOnes = []
imGarbage = False
ignoring = False
stillGarbage = False
for i in range(len(inpt)):
    ig = False
    stillGarbage = False
    c = inpt[i]
    if c == '<' and not imGarbage and not ignoring:
        imGarbage = True
        stillGarbage = True
    elif c == '>' and imGarbage and not ignoring:
        imGarbage = False
        stillGarbage = True
    elif c == '!' and not ignoring:
        ignoring = True
        ig = True

    if not ignoring and not imGarbage and not stillGarbage:
        importantOnes.append(inpt[i])
    elif not stillGarbage and not ig and not ignoring:
        garbageCounter += 1

    if ignoring and not ig:
        ignoring = False

importantOnes = ''.join(importantOnes)

level = 0
score = 0

for i, c in enumerate(importantOnes):
    if c == '{':
        level += 1
        score += level
    elif c == '}':
        level -= 1

print(score)
print(garbageCounter)
