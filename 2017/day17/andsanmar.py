a = [0]
pos = 0
steps = 337
for i in range(2017):
    pos = (pos + steps) % len(a)
    a.insert(pos + 1, i+1)
    pos += 1
print(str(a[a.index(2017)+1]))


curl = 1
pos = 0
out = 0
for i in range(50000000):
    to_ins = i+1
    new = (pos + steps) % curl
    new += 1
    if new == 1:
        out = to_ins
    pos = new
    curl += 1
print(out)
