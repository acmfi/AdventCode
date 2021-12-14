from collections import defaultdict

f = open('input')

input = [line.strip('\n') for line in f.readlines()]

polymers = defaultdict(str)
template = ''
template_dict = defaultdict(int)
for idx, l in enumerate(input):
    if idx > 1:
        pl = l.split(' ')
        polymers[pl[0]] = pl[2]
    elif idx == 0:
        template = l
        for idx in range(len(template)-1):
            template_dict[template[idx:idx+2]] += 1

def add_poly_dict(current_dict):
    new_dict = defaultdict(int)
    for key,val in current_dict.items():
        new_dict[key[0]+polymers[key]] += val
        new_dict[polymers[key]+key[1]] += val
    return new_dict

for _ in range(40):
    template_dict = add_poly_dict(template_dict)
counter_dict = defaultdict(int)
for k,v in template_dict.items():
    counter_dict[k[0]] += v
counter_dict[template[-1]] += 1

MAX = max(counter_dict.values())
MIN = min(counter_dict.values())
print(MAX-MIN)