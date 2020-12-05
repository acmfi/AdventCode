import re

valid = re.compile(r"([a-z])([a-z])\2\1")

data = []
with open("input.txt") as f:
    data = f.readlines()

n = 0
for line in data:
    line = line.strip()
    print(line)
    finds = [m.start(0) for m in valid.finditer(line)]
    finds = [x for x in finds if line[x] != line[x + 1]]
    if len(finds) > 0:
        opens = [m.start(0) for m in re.finditer("\[", line)]
        closes = [m.start(0) for m in re.finditer("\]", line)]
        parens = list(zip(opens, closes))
        ok = True
        for findi in finds:
            print(findi)
            print(parens)
            ok = ok and not any([findi > x and findi < y for (x, y) in parens])
        if ok:
            n += 1
print(n)
