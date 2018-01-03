print("Puedo tardar hasta 11\'")
# inpt = open('input', 'r')
inpt = open('day12/rubenaguadoc/input', 'r')
inpt = inpt.read().rstrip('\n')

inpt = inpt.split('\n')
for idx, val in enumerate(inpt):
    val = val.split(' <->')[1]
    val = [int(x) for x in val.split(', ')]
    inpt[idx] = val

lGroups = 0
groups = []
for i, _ in enumerate(inpt):
    isin = False
    if i in groups:
        continue
    pre = []
    groupZero = [i]

    while not pre == groupZero:
        pre = list(groupZero)
        for idx, val in enumerate(pre):
            groupZero += inpt[val]

        for searchV in pre:
            for idx2, val2 in enumerate(inpt):
                if searchV in val2:
                    groupZero += val2 + [idx2]

        groupZero = sorted(list(set(groupZero)))

    groups += groupZero
    lGroups += 1
    if i == 0:
        print(len(groupZero))

print(lGroups)
