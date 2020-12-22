f = open('input.txt')
data = [int(number) for number in f.readlines()]

idx = 0
jdx = 1
found = False

# First star
while idx < len(data) and jdx < len(data) and not found:
    if data[idx] + data[jdx] == 2020:
        found = True
        print(data[idx] * data[jdx])
    jdx += 1
    if jdx == len(data):
        idx += 1
        jdx = idx + 1

# Second star

idx = 0
jdx = 1
kdx = 2
found = False

while idx < len(data)-2 and jdx < len(data)-1 and kdx < len(data) and not found:
    if data[idx] + data[jdx] + data[kdx] == 2020:
        found = True
        print(data[idx] * data[jdx] * data[kdx])
    kdx += 1
    if kdx == len(data):
        jdx += 1
        kdx = jdx + 1
    if jdx == len(data)-1:
        idx += 1
        jdx = idx + 1
        kdx = jdx + 1