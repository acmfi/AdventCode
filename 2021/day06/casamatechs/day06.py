import numpy as np

f = open('input')
data = [line for line in f.readlines()]

fishes = [int(f) for f in data[0].split(',')]

fishes_spawn = np.zeros(shape=(9,1),dtype=np.int64)

for fish in fishes:
    fishes_spawn[fish] += 1

def calc_fishes(days, fishes):
    for _ in range(days):
        fishes = np.roll(fishes,-1)
        fishes[6] += fishes[-1]
    return fishes.sum()

print(calc_fishes(80, fishes_spawn))
print(calc_fishes(256,fishes_spawn))