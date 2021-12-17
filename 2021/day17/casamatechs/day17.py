from math import sqrt, ceil, floor

f = open('input')

input = [line.strip('\n') for line in f.readlines()][0]

data = input.split(': ')[1].split(', ')
x_target = [int(x) for x in data[0].split('=')[1].split('..')]
y_target = [int(x) for x in data[1].split('=')[1].split('..')]

def make_step(xpos,ypos, xvel, yvel):
    while True:
        xpos += xvel
        ypos += yvel
        if xvel != 0:
            xvel += 1 if xvel < 0 else -1
        yvel -= 1

        if (xpos > x_target[1] and xvel > 0) or (xpos < x_target[0] and xvel == 0) or (ypos < y_target[0] and yvel <= 0):
            return False
        elif x_target[0]<=xpos<=x_target[1] and y_target[0]<=ypos<=y_target[1]:
            return True

def calc_xvel(x_trg):
    return ceil((-1 + sqrt(1+8*x_trg[0]))/2)

y_vel = abs(y_target[0])

while True:
    y_pos = (y_vel**2 + y_vel) / 2
    y_vl = -1
    while y_target[0]<=y_pos:
        if y_pos <= y_target[1] and y_pos + y_vl -1 < y_target[0]:
            break
        y_pos += y_vl
        y_vl -= 1
    if y_pos >= y_target[0]:
        y_vel -= 1
    else:
        break
y_vel -= 1
print((y_vel**2 + y_vel) // 2)

good_throws = 0
for x in range(calc_xvel(x_target),x_target[1]+1):
    for y in range(y_target[0],abs(y_target[0])+1):
        xp,yp,xvel,yvel = 0,0,x,y
        if make_step(xp,yp,xvel,yvel):
            good_throws += 1

print(good_throws)