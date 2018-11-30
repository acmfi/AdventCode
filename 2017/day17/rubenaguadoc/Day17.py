print('May take a while... (Up to 35\')')
inpt = 329
# inpt = 3

vortex = [0]
pos = 0
count = 1

for _ in range(2017):
    pos = (pos + inpt) % len(vortex) + 1
    vortex.insert(pos, count)
    count += 1

print(vortex[pos + 1])

lenVortex = 1
pos = 0
count = 1

for _ in range(50000000):
    pos = (pos + inpt) % lenVortex + 1
    if pos == 1:
        res = count
    lenVortex += 1
    count += 1

print(res)
