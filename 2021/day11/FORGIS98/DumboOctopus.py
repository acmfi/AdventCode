def print_matrix(matrix):
    for i in matrix:
        print(i)
    print("")


def is_valid(matrix, x, y):
    return x >= 0 and y >= 0 and x < len(matrix) and y < len(matrix)


def flash(matrix, posx, posy):
    for i in range(posx - 1, posx + 2):
        for t in range(posy - 1, posy + 2):
            if (i == posx and t == posy):
                matrix[i][t] = 0

            elif (is_valid(matrix, i, t) and matrix[i][t] != 0):
                matrix[i][t] += 1


def octopus_flash(matrix):
    flashed = False
    total_flash = 0

    for i in range(len(matrix)):
        for y in range(len(matrix)):
            if (matrix[i][y] > 9):
                flash(matrix, i, y)
                total_flash += 1
                flashed = True

    return (flashed, total_flash)


def first_star(matrix):
    total = 0
    for _ in range(100):
        matrix = list(map(lambda x: list(map(lambda y: y + 1, x)), matrix))
        flashed = True
        while (flashed):
            (flashed, num_flash) = octopus_flash(matrix)
            total += num_flash

    print("First Star:", total)

def second_star(matrix):
    step = 0
    while(True):
        matrix = list(map(lambda x: list(map(lambda y: y + 1, x)), matrix))
        flashed = True
        while (flashed):
            (flashed, _) = octopus_flash(matrix)

        step += 1
        
        if(sum(sum(elem) for elem in matrix) == 0):
            break

    print("Second Star:", step)


if __name__ == "__main__":
    with open("/home/jorge/input.txt") as file:
        matrix = [[int(num) for num in line.strip()] for line in file]

    second_matrix = matrix[:]
    first_star(matrix)
    second_star(second_matrix)
