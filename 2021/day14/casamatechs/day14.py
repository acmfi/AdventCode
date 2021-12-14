from collections import defaultdict

f = open('input')

input = [line.strip('\n') for line in f.readlines()]

polymers = defaultdict(str)
instertions = defaultdict(int)
template = ''
template_dict = defaultdict(int)
for idx, l in enumerate(input):
    if idx > 1:
        pl = l.split(' ')
        polymers[pl[0]] = pl[2]
    elif idx == 0:
        template = l
        for char in template:
            instertions[char] += 1
        for idx in range(len(template)-1):
            template_dict[template[idx:idx+2]] += 1

def add_poly(current_polymer):
    new_poly = current_polymer
    ins = 0
    for idx in range(len(current_polymer)-1):
        pair = current_polymer[idx]+current_polymer[idx+1]
        if pair in polymers.keys():
            new_poly = new_poly[:idx+ins+1] + polymers[pair] + new_poly[idx+ins+1:]
            instertions[polymers[pair]] += 1
            ins += 1
    return new_poly

def add_poly_dict(current_dict):
    new_dict = defaultdict(int)
    for key in current_dict.keys():
        if key in polymers:
            new_dict[key[0]+polymers[key]] += current_dict[key]
            new_dict[polymers[key]+key[1]] += current_dict[key]
        else:
            new_dict[key] += current_dict[key]
    return new_dict

polymer = template
for _ in range(40):
    template_dict = add_poly_dict(template_dict)
counter_dict = defaultdict(int)
for k,v in template_dict.items():
    counter_dict[k[0]] += v
counter_dict[template[-1]] += 1

MAX = max(counter_dict.values())
MIN = min(counter_dict.values())
print(MAX-MIN)