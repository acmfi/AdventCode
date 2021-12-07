from functools import reduce

f = open('input')
data = [line for line in f.readlines()]

crabs_position = [int(f) for f in data[0].split(',')]

#First star
smaller_pos = None
for i in range(max(crabs_position)):
    aux_pos = 0
    for crab in crabs_position:
        aux_pos += abs(i-crab)
    if not smaller_pos or aux_pos < smaller_pos:
        smaller_pos = aux_pos

print(smaller_pos)

# Second star
smaller_pos = None
for i in range(max(crabs_position)):
    aux_pos = 0
    for crab in crabs_position:
        delta = abs(i-crab)
        if delta > 0:
            fuel_consumption = delta*(delta+1)//2 # Changed the expensive O(n) reduce for O(1) binomial coefficient
            aux_pos += fuel_consumption
    if not smaller_pos or aux_pos < smaller_pos:
        smaller_pos = aux_pos
print(smaller_pos)