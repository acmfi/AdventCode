f = open('input')
data = [line for line in f.readlines()]

inputs = [line.strip('\n\|').split(' ') for line in data]

# First star

cnt = 0
for input in inputs:
    outputs = input[-4:]
    for out in outputs:
        if len(out) < 5 or len(out) == 7:
            cnt += 1

print(cnt)

# Second star

count_2 = 0

nums = {
    'abcefg':0,
    'cf':1,
    'acdeg':2,
    'acdfg':3,
    'bcdf':4,
    'abdfg':5,
    'abdefg':6,
    'acf':7,
    'abcdefg':8,
    'abcdfg':9
    }

def wiring_map(inputs):
    mapped_wires = {
        'a': '',
        'b': '',
        'c': '',
        'd': '',
        'e': '',
        'f': '',
        'g': '',
        }   
    n1 = next(x for x in inputs if len(x) == 2)
    n7 = next(x for x in inputs if len(x) == 3)
    mapped_wires['a'] = list(set(n7) - set(n1))[0]
    n4 = next(x for x in inputs if len(x) == 4)
    n9 = [n for n in inputs if len(n) == 6 and set(n4) < set(n)][0]
    mapped_wires['g'] = list(set(n9) - set(n4) - set(mapped_wires['a']))[0]
    n3 = [n for n in inputs if len(n) == 5 and set(n1).issubset(n)][0]
    mapped_wires['d'] = list(set(n3) - set(n1) - set(mapped_wires['a']) - set(mapped_wires['g']))[0]
    mapped_wires['b'] = list(set(n4) - set(n1) - set(mapped_wires['d']))[0]
    n5 = [n for n in inputs if len(n) == 5 and set([mapped_wires[x] for x in 'abdg']).issubset(n)][0]
    mapped_wires['f'] = list(set(n5) - set([mapped_wires[x] for x in 'abdg']))[0]
    mapped_wires['c'] = list(set(n1) - set(mapped_wires['f']))[0]
    mapped_wires['e'] = list(set('abcdefg') - set([mapped_wires[x] for x in 'abcdfg']))[0]
    return dict((v,k) for k,v in mapped_wires.items())

def output_mapper(outputs, map_dict, cnt):
    aux_cnt = 0
    for output in outputs:
        mapped_output = ''.join(sorted([map_dict[x] for x in output]))
        aux_cnt = aux_cnt * 10 + nums[mapped_output]
    return cnt + aux_cnt

for i in range(len(inputs)):
    wires_map = wiring_map(inputs[i][:10])
    count_2 = output_mapper(inputs[i][-4:], wires_map, count_2)

print(count_2)
