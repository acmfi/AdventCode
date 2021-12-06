def genboard(ini, end, board):
    for i, e in zip(ini, end):
        if i[0] == e[0]:
            if i[1] > e[1]:
                for j in range((i[1] - e[1])+1):
                    board[i[0]][e[1]+j] += 1
            else:
                for j in range((e[1] - i[1])+1):
                    board[i[0]][i[1]+j] += 1
        elif i[1] == e[1]:
            if i[0] > e[0]:
                for j in range((i[0] - e[0])+1):
                    board[e[0]+j][i[1]] += 1
            else:
                for j in range((e[0] - i[0])+1):
                    board[i[0]+j][i[1]] += 1
    return board

def genboard2(ini, end, board):
    for i, e in zip(ini, end):
        if i[0] == e[0]:
            if i[1] > e[1]:
                for j in range((i[1] - e[1])+1):
                    board[i[0]][e[1]+j] += 1
            else:
                for j in range((e[1] - i[1])+1):
                    board[i[0]][i[1]+j] += 1
        elif i[1] == e[1]:
            if i[0] > e[0]:
                for j in range((i[0] - e[0])+1):
                    board[e[0]+j][i[1]] += 1
            else:
                for j in range((e[0] - i[0])+1):
                    board[i[0]+j][i[1]] += 1
        elif abs(e[0]-i[0]) == abs(e[1]-i[1]):
            stepx = 1 if e[0]-i[0] > 0 else -1
            stepy = 1 if e[1]-i[1] > 0 else -1
            e[0] += 1 if e[0]-i[0] > 0 else -1
            for _ in range(abs(e[0]-i[0])):
                board[i[0]][i[1]] += 1
                i[0] += stepx
                i[1] += stepy

    return board

def overloaps(board):
    return sum(1 for row in board for i in row if i > 1)

if __name__ == "__main__":

    with open("./input", "r") as fd:
        data = fd.read().splitlines()

    ini, end = [], []
    for st in data:
        st = st.split("->")
        ini.append(list(map(int, st[0].split(","))))
        end.append(list(map(int, st[1].split(","))))

    x_max = max([max(ini, key=lambda x: x[0])[0], max(end, key=lambda x: x[0])[0]])
    y_max = max([max(ini, key=lambda x: x[1])[1], max(end, key=lambda x: x[1])[1]])

    board = [[0 for _ in range(y_max+1)] for _ in range(x_max+1)]
    print("First:", overloaps(genboard(ini, end, board)))

    board = [[0 for _ in range(y_max+1)] for _ in range(x_max+1)]
    print("Second:", overloaps(genboard2(ini, end, board)))