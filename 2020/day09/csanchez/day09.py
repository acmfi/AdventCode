f = open('input.txt')
lines = [int(x) for x in f.read().split('\n')]

left_idx = 0
right_idx = 1
look_idx = 25

while right_idx < look_idx:
    if lines[left_idx] + lines[right_idx] == lines[look_idx]:
        look_idx += 1
        left_idx = look_idx-25
        right_idx = look_idx-24
    else:
        if look_idx - right_idx == 1:
            left_idx += 1
            right_idx = left_idx + 1
        else:
            right_idx += 1

invalid_number = lines[look_idx]
print('First star solution:', invalid_number)

init_idx = 0
set_idx = 0
accum = 0
solution = 0

while True:
    accum += lines[set_idx]
    if accum == invalid_number:
        solution_set = lines[init_idx:set_idx]
        solution = min(solution_set) + max(solution_set)
        break
    elif accum > invalid_number:
        init_idx += 1
        set_idx = init_idx
        accum = 0
    else:
        set_idx += 1
    
print('Second star solution:', solution)