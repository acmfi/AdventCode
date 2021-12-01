f = open('input.txt')

data = f.read().splitlines()
valid_pass = 0

# First star
for line in data:
    members = line.split(' ')
    min = int(members[0].split('-')[0])
    max = int(members[0].split('-')[1])
    letter = members[1][0]
    string = members[2]
    if string.count(letter) >= min and string.count(letter) <= max:
        valid_pass += 1

print(valid_pass)

# Second star

new_valid_pass = 0

for line in data:
    members = line.split(' ')
    pos1 = int(members[0].split('-')[0])-1
    pos2 = int(members[0].split('-')[1])-1
    letter = members[1][0]
    string = members[2]
    if (string[pos1] == letter and string[pos2] != letter) or (string[pos1] != letter and string[pos2] == letter):
        new_valid_pass += 1

print(new_valid_pass)