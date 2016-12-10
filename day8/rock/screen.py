def init_screen():
    return [[0 for _ in range(50)] for _ in range(6)]


def print_screen(screen):
    for row in screen:
        for x in row:
            print(".", end='') if x == 0 else print("#", end='')
        print()


def rect(screen, wide, tall):
    for i in range(tall):
        for j in range(wide):
            screen[i][j] = 1
    return screen


def rotate_right(row):
    return [row[-1]] + row[:-1]


def rotate_row(screen, r, n):
    for _ in range(n):
        screen[r] = rotate_right(screen[r])
    return screen


def transpose(screen):
    return list(map(list, zip(*screen)))


def rotate_column(screen, r, n):
    return transpose(rotate_row(transpose(screen), r, n))


def n_on(screen):
    n = 0
    for row in screen:
        for x in row:
            if x > 0:
                n += 1
    return n


def act_line(screen, line):
    line = line.split()
    if line[0] == "rect":
        x, y = line[1].split("x")
        return rect(screen, int(x), int(y))
    if line[0] == "rotate":
        if line[1] == "row":
            r = int(line[2].split("=")[1])
            n = int(line[-1])
            return rotate_row(screen, r, n)
        if line[1] == "column":
            r = int(line[2].split("=")[1])
            n = int(line[-1])
            return rotate_column(screen, r, n)
    return screen


data = []
with open("input.txt") as f:
    data = f.readlines()

screen = init_screen()

for line in data:
    screen = act_line(screen, line)

print_screen(screen)
print(n_on(screen))
