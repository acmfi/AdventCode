import copy
input = [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6]
seen = []
steps = 0
while input not in seen:
    seen.append(copy.deepcopy(input))
    acum = max(input)
    index = input.index(acum)
    input[index] = 0
    index = (index + 1) % len(input)
    while acum != 0:
        input[index] = input[index] + 1
        acum = acum - 1
        index = (index + 1) % len(input)
    steps = steps + 1
print(steps)
print(steps-seen.index(input))
