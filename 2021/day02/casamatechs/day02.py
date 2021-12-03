f = open('input')
data = [line for line in f.readlines()]

def movementAxis(data):
    x = 0
    y = 0
    for text, num in [l.split(' ') for l in data]:
        num = int(num)
        if text == 'forward':
            x += num
        elif text == 'down':
            y += num 
        else:
            y -= num
    print(x*y)

movementAxis(data)

def movementAim(data):
    x = 0
    y = 0
    aim = 0
    for text, num in [l.split(' ') for l in data]:
        num = int(num)
        if text == 'forward':
            x += num
            y += aim * num
        elif text == 'down':
            aim += num 
        else:
            aim -= num
    print(x*y)

movementAim(data)