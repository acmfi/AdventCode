print('May take up to 45\'')
# inpt = open('input', 'r')
inpt = open('day13/rubenaguadoc/input', 'r')
inpt = inpt.read().rstrip('\n')
inpt = inpt.split('\n')
lengths = {int(x.split(': ')[0]): (int(x.split(': ')[1]) - 1) * 2 for x in inpt}
severity = None
count = 0

while not severity == 0:
    severity = 0
    for i in range(max(lengths.keys()) + 1):
        if i in lengths.keys():
            if (i + count) % lengths[i] == 0:
                if count == 0:
                    severity += int(i * (lengths[i] / 2 + 1))
                else:
                    severity = 1
                    break

    if count == 0:
        print(severity)

    count += 1

print(count - 1)
# 259572
# 3943252
