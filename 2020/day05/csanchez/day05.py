f = open('input.txt')

puzzle = f.read().split('\n')

# According to the script, the given string can be converted directly to a binary number. int(x,2) converts a binary string to a decimal integer.
unique_ids = [int(x.replace('F', '0').replace('B', '1').replace('R', '1').replace('L', '0'),2) for x in puzzle]
print('First star solution: ', max(unique_ids))

unique_ids_sorted = sorted(unique_ids)

right_idx = 1
seat_id = -1
while right_idx < len(unique_ids_sorted):
    if unique_ids_sorted[right_idx] - unique_ids_sorted[right_idx-1] == 2:
        seat_id = unique_ids_sorted[right_idx]-1
        break
    right_idx +=1

print('Second star solution: ', seat_id)