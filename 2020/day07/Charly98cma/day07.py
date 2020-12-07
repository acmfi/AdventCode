import sys

bags = ['shiny gold']
rules = [rule.split(' contain ') for rule in open(sys.argv[1],'r').read().replace('.','').replace(' bags','').split('\n')[:-1]]

for bag in bags:
    for x in rules:
        if (bag in x[1] and x[0] not in bags):
            bags.append(x[0])
print("1st STAR SOLUTION ->", len(bags)-1)
