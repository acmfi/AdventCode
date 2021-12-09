import math

f = open('input')
data = [line for line in f.readlines()]

inputs = [line.strip('\n') for line in data]

matrix = []

for idx, line in enumerate(inputs):
    matrix += [[int(x) for x in line]]
counter = 0

low_points = []

# First star
for idx, i_line in enumerate(matrix):
    for jdx, j_line in enumerate(i_line):
        if idx != 0 and idx != len(matrix)-1:
            if jdx != 0 and jdx != len(i_line)-1:
                if j_line < matrix[idx-1][jdx] and j_line < matrix[idx+1][jdx] and j_line < matrix[idx][jdx-1] and j_line < matrix[idx][jdx+1]:
                    counter += j_line + 1
                    low_points += [(idx,jdx)]
            elif jdx == 0:
                if j_line < matrix[idx-1][0] and j_line < matrix[idx+1][0] and j_line < matrix[idx][1]:
                    counter += j_line + 1
                    low_points += [(idx,jdx)]
            else:
                if j_line < matrix[idx][len(i_line)-2] and j_line < matrix[idx-1][len(i_line)-1] and j_line < matrix[idx+1][len(i_line)-1]:
                    counter += j_line + 1
                    low_points += [(idx,jdx)]
        elif idx == 0:
            if jdx != 0 and jdx != len(i_line)-1:
                if j_line < matrix[0][jdx-1] and j_line < matrix[0][jdx+1] and j_line < matrix[1][jdx]:
                    counter += j_line + 1
                    low_points += [(idx,jdx)]
            elif jdx == 0:
                if j_line < matrix[0][1] and j_line < matrix[1][0]:
                    counter += j_line + 1
                    low_points += [(idx,jdx)]
            else:
                if j_line < matrix[0][len(i_line)-2] and j_line < matrix[1][len(i_line)-1]:
                    counter += j_line + 1
                    low_points += [(idx,jdx)]
        else:
            if jdx != 0 and jdx != len(i_line)-1:
                if j_line < matrix[idx][jdx-1] and j_line < matrix[idx][jdx+1] and j_line < matrix[idx-1][jdx]:
                    counter += j_line + 1
                    low_points += [(idx,jdx)]
            elif jdx == 0:
                if j_line < matrix[idx][1] and j_line < matrix[idx-1][0]:
                    counter += j_line + 1
                    low_points += [(idx,jdx)]
            else:
                if j_line < matrix[idx][len(i_line)-2] and j_line < matrix[idx-1][len(i_line)-1]:
                    counter += j_line + 1
                    low_points += [(idx,jdx)]

print(counter)

def calc_basin_size(idx, jdx):
    if (idx,jdx) in visited_points or matrix[idx][jdx] == 9:
        return
    else:
        visited_points.add((idx,jdx))
        if idx != 0 and idx != len(matrix)-1 and jdx != 0 and jdx != len(matrix[0])-1:
            calc_basin_size(idx-1, jdx)
            calc_basin_size(idx+1, jdx)
            calc_basin_size(idx, jdx-1)
            calc_basin_size(idx, jdx+1)
        elif jdx != 0 and jdx != len(matrix[0])-1:
            if idx == 0:
                calc_basin_size(idx, jdx -1)
                calc_basin_size(idx,jdx+1)
                calc_basin_size(idx+1,jdx)
            else:
                calc_basin_size(idx, jdx -1)
                calc_basin_size(idx,jdx+1)
                calc_basin_size(idx-1,jdx)
        else:
            if idx != 0 and idx != len(matrix)-1 and jdx == 0:
                calc_basin_size(idx-1, jdx)
                calc_basin_size(idx + 1, jdx)
                calc_basin_size(idx, jdx+1)
            elif idx != 0 and idx != len(matrix)-1 and jdx == len(matrix[0])-1:
                calc_basin_size(idx-1, jdx)
                calc_basin_size(idx + 1, jdx)
                calc_basin_size(idx, jdx-1)
            elif idx == 0 and jdx == 0:
                calc_basin_size(idx,jdx+1)
                calc_basin_size(idx+1,jdx)
            elif idx == 0 and jdx == len(matrix[0])-1:
                calc_basin_size(idx,jdx-1)
                calc_basin_size(idx+1,jdx)
            elif idx == len(matrix)-1 and jdx == 0:
                calc_basin_size(idx-1,jdx)
                calc_basin_size(idx,jdx+1)
            else:
                calc_basin_size(idx-1,jdx)
                calc_basin_size(idx,jdx-1)

basin_size = [0] * 3
for low_point in low_points:
    visited_points = set()
    idx, jdx = low_point
    calc_basin_size(idx,jdx)
    if basin_size[0] < len(visited_points):
        basin_size[0] = len(visited_points)
        basin_size.sort()
print(math.prod(basin_size))