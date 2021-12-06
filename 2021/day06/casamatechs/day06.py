import numpy as np

f = open('input')
data = [line for line in f.readlines()]

fishes = [int(f) for f in data[0].split(',')]

fishes_spawn = np.zeros(shape=(9,1),dtype=np.int64)

for fish in fishes:
    fishes_spawn[fish] += 1

for i in range(256):
    fishes_spawn = np.roll(fishes_spawn,-1)
    fishes_spawn[6] += fishes_spawn[-1]
print(fishes_spawn.sum())