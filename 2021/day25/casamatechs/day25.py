f = open('input')

input = [line.strip('\n') for line in f.readlines()]

matrix = {}
for idx,line in enumerate(input):
    for jdx,char in enumerate(line):
        matrix[(idx,jdx)] = char

def check_east():
    return [k for k,v in matrix.items() if v == '>' and matrix[(k[0],(k[1]+1) % len(input[0]))] == '.']

def check_south():
    return [k for k,v in matrix.items() if v == 'v' and matrix[((k[0]+1) % len(input),k[1])] == '.']

movements_available = True
step = 0

while movements_available:
    movements_available = False
    for k in check_east():
        matrix[k] = '.'
        matrix[(k[0],(k[1]+1) % len(input[0]))] = '>'
        movements_available = True
    for k in check_south():
        matrix[k] = '.'
        matrix[((k[0]+1) % len(input),k[1])] = 'v'
        movements_available = True
    step += 1

print(step)