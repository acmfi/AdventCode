import copy
import functools

f = open('input.txt')

lines = f.read().split('\n')

gold_bags = list(filter(lambda x: 'shiny gold bag' in x, lines))

upper_bags = []
for l in gold_bags:
    container = l.split(' contain')[0][:-5]
    if container not in 'shiny gold bags' and container not in upper_bags:
        upper_bags.append(container)

aux_upper_bags = copy.deepcopy(upper_bags)

while len(aux_upper_bags) != 0:
    containers = list(filter(lambda x: aux_upper_bags[0] in x, lines))
    for l in containers:
        container = l.split(' contain')[0][:-5]
        if aux_upper_bags[0] not in container and container not in upper_bags:
            upper_bags.append(container)
            aux_upper_bags.append(container)
    aux_upper_bags = aux_upper_bags[1:]

print('First star solution:', len(upper_bags))

bags_tree = {}

for l in lines:
    container = l.split(' contain ')[0]
    contained = l.split(' contain ')[1].split(', ')
    bags_tree[container] = contained

def calc_tree(input):
    total = 0
    for l in input:
        if l == 'no other bags.':
            return 1
        amount = int(l.split(' ')[0])
        bags = str(functools.reduce(lambda a,b: a+' '+b, l.split(' ')[1:]))
        if bags[-1] == '.':
            bags = bags[:-1]
        if bags[-1] != 's':
            bags = bags+'s'
        total += amount * calc_tree(bags_tree[bags])
        #This if is necessary because we need to count the bags containing other bags
        if bags_tree[bags][0] != 'no other bags.':
            total += amount
    return total

print('Second star solution:', calc_tree(bags_tree['shiny gold bags']))