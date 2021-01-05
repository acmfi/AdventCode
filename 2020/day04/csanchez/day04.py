import re

f = open('input.txt')

puzzle = f.read().split('\n\n')
puzzle = list(map(lambda line: line.replace('\n', ' '), puzzle))

required_fields = {
    'byr' : '^(19[2-9][0-9]|200[0-2])$',
    'iyr' : '^20(1[0-9]|20)$',
    'eyr' : '^20(2[0-9]|30)$',
    'hgt' : '^((59|6[0-9]|7[0-6])in|(1[5-8][0-9]|19[0-3])cm)$',
    'hcl' : '^#[0-9a-f]{6}$',
    'ecl' : '^(amb|blu|brn|gry|grn|hzl|oth)$',
    'pid' : '^[0-9]{9}$'
    }

fields_puzzle = list(map(lambda line: dict(map(lambda ln : (ln.split(':')[0], ln.split(':')[1]), line.split(' '))), puzzle))

valid_passports = list(filter(lambda fields: set(required_fields).issubset(fields.keys()), fields_puzzle))

print('First star solution: ', len(valid_passports))

# It's solved with regex cuz yolo
def checkRegex(fields):
    for f in required_fields.keys():
        regex = re.compile(required_fields[f])
        if not regex.fullmatch(fields[f]):
            return False
    return True

data_valid_passports = list(filter(checkRegex, valid_passports))

print('Second star solution: ', len(data_valid_passports))