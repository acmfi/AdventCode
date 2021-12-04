import numpy as np

f = open('input')
data = [line for line in f.readlines()]

draw_idx = 0
final_number = 0
drawed_numbers = [int(num) for num in data[0].split(',')]

def build_boards(data):
    boards = []
    idx = 2
    while idx < len(data):
        board = np.zeros(shape=(5,5), dtype=np.int32)
        for i in range(5):
            line = [x for x in data[idx].split(' ') if x.strip()]
            for j in range(5):
                board[i][j] = int(line[j])
            idx += 1
        idx += 1
        boards.append(board)
    return boards

def check_number_in_boards(boards, number):
    for board in boards:
        if type(board) is not int and number in board: # Had to add type check here to complete 2nd star
            board[np.where(board == number)] = -1
    return boards

def check_winner(boards):
    for board in boards:
        for row in range(5):
            if np.sum(board[row,:]) == -5:
                return board
        for col in range(5):
            if np.sum(board[:,col]) == -5:
                return board
    return None

def check_winner_and_discard(boards, win, nums, win_number):
    for idx, board in enumerate(boards):
        if type(board) is not int:
            for row in range(5):
                if np.sum(board[row,:]) == -5:
                    boards[idx] = -1
                    win = board
                    win_number = nums
                    break
            for col in range(5):
                if np.sum(board[:,col]) == -5:
                    boards[idx] = -1
                    win = board
                    win_number = nums
                    break
    return boards, win, win_number

brd = build_boards(data)

for nums in drawed_numbers:
    brd = check_number_in_boards(brd, nums)
    winner = check_winner(brd)
    if winner is not None:
        result = np.sum([n for n in winner.flatten() if n >= 0]) * nums
        print(result)
        break

brd = build_boards(data)
winner = None
winning_num = -1
for nums in drawed_numbers:
    brd = check_number_in_boards(brd, nums)
    brd, winner, winning_num = check_winner_and_discard(brd, winner, nums, winning_num)

result = np.sum([n for n in winner.flatten() if n >= 0]) * winning_num
print(result)

