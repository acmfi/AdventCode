from functools import reduce
from operator import iconcat

def mgenerator(data):
    matrix, newM = [], []
    for line in data:
        if line == "":
            matrix.append(newM)
            newM = []
        else:
            newM.append(list(map(int, line.split())))
    return matrix

def change_num(matrix, num):
    new_matrix = []
    for mat in matrix:
        new_mat = []
        for row in mat:
            new_mat.append([i if i != num else -1 for i in row])
        new_matrix.append(new_mat)
    return new_matrix

def check_win(matrix):
    for mat in matrix:
        if any([sum(col) == -5 for col in zip(*mat)] + [sum(row) == -5 for row in mat]):
            return sum(i for i in reduce(iconcat, mat) if i != -1)
    return 0

def play(numbers, matrix):
    for i in numbers:
        matrix = change_num(matrix, i)
        if (win := check_win(matrix)) != 0:
            return win * i
    return 0

def play2(numbers, matrix):
    for i in numbers:
        matrix = change_num(matrix, i)
        if isinstance((win := check_win2(matrix)), int):
            return win * i
        else:
            matrix = win
    return 0

def check_win2(matrix):
    new_matrix = []
    for mat in matrix:
        if any([sum(col) == -5 for col in zip(*mat)] + [sum(row) == -5 for row in mat]):
            if len(matrix) == 1:
                return sum(i for i in reduce(iconcat, mat) if i != -1)
        else:
            new_matrix.append(mat)
    return new_matrix

if __name__ == "__main__":

    with open("./input", "r") as fd:
        data = fd.read().splitlines()
    
    numbers = list(map(int, data[0].split(",")))
    matrix = mgenerator(data[2:])

    print("First:", play(numbers, matrix))
    print("Second:", play2(numbers, matrix))