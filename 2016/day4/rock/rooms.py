import re
from collections import Counter
import itertools

roomreg = re.compile("(([a-z]+-)+)([0-9]+)\[([a-z]+)\]")

def extract_data(line):
    grs = roomreg.search(line).groups()
    return "".join(grs[0].split("-")), int(grs[2]), grs[3]


def get_hash(d):
    c = Counter(d).most_common()
    return "".join([''.join(sorted([x[0] for x in g])) for _, g in  itertools.groupby(c, lambda x: x[1])])[:5]


data = []
with open("input.txt") as f:
    data = [extract_data(x) for x in f.readlines()]


# testin = ["aaaaa-bbb-z-y-x-123[abxyz]", "a-b-c-d-e-f-g-h-987[abcde]", "not-a-real-room-404[oarel]", "totally-real-room-200[decoy]"]
# data = [extract_data(x) for x in testin]

sumn = 0
for d in data:
    hashd = get_hash(d[0])
    if hashd == d[2]:
        sumn += d[1]

print(sumn)
