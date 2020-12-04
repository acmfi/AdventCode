from string import hexdigits


req_fields = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']


def passports():
    with open('day04.input', 'r') as f:
        passport = {}
        while (l := f.readline()) != '':
            l = l.replace('\n', '')
            if l != '':
                l = l.split(' ')
                l = [field.split(':') for field in l]
                for field in l:
                    passport[field[0]] = field[1]
            else:
                yield passport
                passport = {}   # I didn't know you could have shit AFTER 'yield' but you can.
        yield passport


def print_passport(print_passport):
    for f in req_fields:
        try:
            print(f, ': ', print_passport[f])
        except: 
            print(f, ':  MISSING')
    print()


def valid_star1(passport):
    return all(field in passport.keys() for field in req_fields)


def star1():
    total = 0
    for passport in passports():
        total += valid_star1(passport)
    return total


star2_functions = {'byr': lambda x : int(x) in range(1920, 2003),
                   'iyr': lambda x : int(x) in range(2010, 2021),
                   'eyr': lambda x : int(x) in range(2020, 2031),
                   'hgt': lambda x : int(x[0]) in range(x[1], x[2]),
                   'hcl': lambda x : all(c in hexdigits[:-6] for c in x),
                   'ecl': lambda x : x in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'],
                   'pid': lambda x : all(int(i) in range(0, 10) for i in x)
                   }


def valid_star2(passport):
    res = True
    for field in req_fields:
        x = passport[field]
        if field == 'hgt':
            if x[-2:] == 'in':
                x = [x[:-2], 59, 77]
            elif x[-2:] == 'cm':
                x = [x[:-2], 150, 194]
            else:
                res = False
        elif field == 'hcl':
            if x[0] == '#':
                x = x[1:]
            else:
                res = False
        elif field == 'pid':
            if len(x) != 9:
                res = False
        try:
            res &= star2_functions[field](x)
        except:
            res = False
        if not res:
            break
    return res


def star2():
    total = 0
    for passport in passports():
        fieldnames = passport.keys()
        correct = valid_star1
        if valid_star1(passport):
            total += valid_star2(passport)
    return total

print('STAR 1:', star1())
print('STAR 2:', star2())
