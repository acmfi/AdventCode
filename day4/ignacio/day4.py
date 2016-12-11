import re
from collections import Counter
from operator import itemgetter
from functools import reduce

r = re.compile('([a-z]+)([0-9]+)\[([a-z]+)\]')

def parse_room(line):
    line_no_dash = line.replace('-','')
    return re.match(r,line_no_dash).groups()

def most_common(word):
    mc = Counter(word).most_common()
    s0 = sorted(mc, key=itemgetter(0))
    s1 = sorted(s0, key=itemgetter(1), reverse = True)
    return reduce(lambda c1, c2: c1+c2,map(lambda t: t[0],s1[:5]))
    
n = 0
with open('input.txt','r') as rooms:
    for line in rooms:
        word, number, hs = parse_room(line)
        mc = most_common(word)
        if hs == mc:
            n += int(number)

print('The answer is always',n)
