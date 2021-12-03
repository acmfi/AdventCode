import numpy as np

f = open('input')
data = [int(number) for number in f.readlines()]

def countIncrements(data):
    cnt = 0
    for idx in range(len(data)-1):
        if data[idx+1] > data[idx]:
            cnt += 1
    print(cnt)

countIncrements(data)

def windowIncrements(data):
    cnt = 0
    for idx in range(len(data)-3):
        a = np.sum(data[idx:idx+3])
        b = np.sum(data[idx+1:idx+4])
        if b > a:
            cnt += 1
    print(cnt)

windowIncrements(data)