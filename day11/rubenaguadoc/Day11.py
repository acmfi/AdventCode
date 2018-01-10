inpt = open('input', 'r')
# inpt = open('day11/rubenaguadoc/input', 'r')
inpt = inpt.read().rstrip('\n')
inpt = inpt.split(',')


def calculateSteps(inpt):
    directions = {'n': 0, 'ne': 0, 'se': 0, 's': 0, 'sw': 0, 'nw': 0}
    for direc in inpt:
        directions[direc] += 1

    stepsNorth = directions['n'] - directions['s']
    stepsNorthEast = directions['ne'] - directions['sw']
    stepsNorthWest = directions['nw'] - directions['se']

    width = stepsNorthEast - stepsNorthWest
    heigth = stepsNorth + stepsNorthEast / 2 + stepsNorthWest / 2
    if abs(heigth) > abs(width):
        finalSteps = abs(width) + abs(abs(heigth) - abs(width) * 0.5)
    else:
        finalSteps = abs(width)
    return int(finalSteps)


print(calculateSteps(inpt))

maxStepsAway = 0
for idx, _ in enumerate(inpt):
    res = calculateSteps(inpt[:idx + 1:1])
    if res > maxStepsAway:
        maxStepsAway = res

print(maxStepsAway)
