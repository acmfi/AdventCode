import numpy as np

f = open('input')
raw_data = [line.split(' ') for line in f.readlines()]

wind_structure = np.zeros(shape=(len(raw_data),4), dtype=np.int32)

for idx, wind in enumerate(raw_data):
    wind_1 = wind[0].split(',')
    wind_2 = wind[2].split(',')
    for jdx, val in enumerate(wind_1 + wind_2):
        wind_structure[idx,jdx] = int(val)

max_x = wind_structure[:,[0,2]].max()
max_y = wind_structure[:,[1,3]].max()

wind_diagram = np.zeros(shape=(max_x+1,max_y+1), dtype=np.int32)

def include_record(x1, y1, x2, y2):
    if x1 == x2:
        if y1 > y2:
            wind_diagram[y2:y1+1,x1] += 1
        else:
            wind_diagram[y1:y2+1,x1] += 1
    else:
        if x1 > x2:
            wind_diagram[y1,x2:x1+1] += 1
        else:
            wind_diagram[y1,x1:x2+1] += 1

def include_diagonal_record(x1,y1,x2,y2):
    x_delta = x2-x1
    y_delta = y2-y1
    x2 = x2+1 if x_delta > 0 else x2-1 # Because range function is [)
    x_step = 1 if x_delta > 0 else -1
    y_step = 1 if y_delta > 0 else -1
    for i in range(x1,x2,x_step):
        wind_diagram[y1,i] += 1
        y1 += y_step

for wind in wind_structure:
    x1, y1, x2, y2 = wind
    if x1 == x2 or y1 == y2:
        include_record(x1,y1,x2,y2)
        # Until this point, code is valid for 1st star
    elif abs(x2-x1) == abs(y2-y1):
        include_diagonal_record(x1,y1,x2,y2)

print((wind_diagram > 1).sum())