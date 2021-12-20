f = open('input')

input = [line.strip('\n') for line in f.readlines()]

algorithm = input[0]

matrix = set()
for i in range(2,len(input)):
    for j, char in enumerate(input[i]):
        if char == '#':
            matrix.add((j,i))

def calc_pixel_enhancement(matrix,all_off):
    new_matrix = set()
    x_min = min([x[0] for x in matrix])
    x_max = max([x[0] for x in matrix])
    y_min = min([x[1] for x in matrix])
    y_max = max([x[1] for x in matrix])
    for xx in range(x_min-1,x_max+2):
        for yy in range(y_min-1,y_max+2):
            pixel=(xx,yy)
            result = ''
            x = [-1,0,1]*3
            y = [-1,-1,-1,0,0,0,1,1,1]
            for i in range(9):
                px = pixel[0]+x[i]
                py = pixel[1]+y[i]
                result += '1' if ((px,py) in matrix) == all_off else '0'
            pos_alg = int(result,2)
            if (algorithm[pos_alg] == '#') != all_off:
                new_matrix.add((px,py))
    return new_matrix

def show(matrix):
    x_min = min([x[0] for x in matrix])
    x_max = max([x[0] for x in matrix])
    y_min = min([x[1] for x in matrix])
    y_max = max([x[1] for x in matrix])
    for dy in range(y_min-5,y_max+5):
        row=''
        for dx in range(x_min-5,x_max+5):
            row += '.' if (dx,dy) not in matrix else '#'
        print(row)
    print('')

for i in range(50):
    matrix = calc_pixel_enhancement(matrix,i%2==0)
print(len(matrix))