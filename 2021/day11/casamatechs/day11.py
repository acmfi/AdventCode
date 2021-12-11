from copy import deepcopy

f = open('input')
matrix = []
for line in [list(line.strip()) for line in f.readlines()]:
    matrix += [[int(x) for x in line]]

flashing_x = [0,0,1,1,1,-1,-1,-1]
flashing_y = [1,-1,1,0,-1,1,0,-1]

def check_flash(mat,row,col,cnt):
    if mat[row][col] >= 10:
        cnt += 1
        mat[row][col] = -1
        for i in range(len(flashing_x)):
            r = row+flashing_x[i]
            c = col+flashing_y[i]
            if 0<=r<len(mat) and 0<=c<len(mat[0]) and mat[r][c] != -1:
                mat[r][c] += 1
                cnt,mat = check_flash(mat,r,c,cnt)
    return cnt,mat

def step(n, mat):
    counter = 0
    for _ in range(n):
        for row in range(len(mat)):
            for col in range(len(mat[0])):
                mat[row][col] += 1
        for row in range(len(mat)):
            for col in range(len(mat[0])):
                counter,mat = check_flash(mat,row,col,counter)
        for row in range(len(mat)):
            for col in range(len(mat[0])):
                if mat[row][col] == -1:
                    mat[row][col] = 0
    return mat,counter

print(step(100, deepcopy(matrix))[1])

all_flash = False
attempts = 0
m = deepcopy(matrix)
while not all_flash:
    m,_ = step(1,m)
    attempts += 1
    if sum([x for nums in m for x in nums]) == 0:
        break
print(attempts)