p = {'a': 0, 'b': 1, 'c': 2, 'd': 3, 'e': 4, 'f': 5,
     'g': 6, 'h': 7, 'i': 8, 'j': 9, 'k': 10, 'l': 11,
     'm': 12, 'n': 13, 'o': 14, 'p': 15}


def spin(num):
    for x in p.keys():
        p[x] = (p[x] + num) % 16


def partner(l1, l2):
    i = p[l1]
    p[l1] = p[l2]
    p[l2] = i


def exchange(n1, n2):
    i1 = ""
    i2 = ""
    for x in p.keys():
        if p[x] == n1:
            i1 = x
        if p[x] == n2:
            i2 = x
    partner(i1, i2)


with open("input", "r") as file:
    for line in file:
        e = line.split(',')
        for k in e:
            if k[0] == 'x':
                # print(k)
                a = k[1:].split('/')
                exchange(int(a[0]), int(a[1]))
            elif k[0] == 's':
                spin(int(k[1:]))
            elif k[0] == 'p':
                a = k[1:].split('/')
                partner(a[0], a[1])

print(str(p))

p = {'a': 0, 'b': 1, 'c': 2, 'd': 3, 'e': 4, 'f': 5,
     'g': 6, 'h': 7, 'i': 8, 'j': 9, 'k': 10, 'l': 11,
     'm': 12, 'n': 13, 'o': 14, 'p': 15}
p2 = {'a': 0, 'b': 1, 'c': 2, 'd': 3, 'e': 4, 'f': 5,
      'g': 6, 'h': 7, 'i': 8, 'j': 9, 'k': 10, 'l': 11,
      'm': 12, 'n': 13, 'o': 14, 'p': 15}

# Lets see the mooves simplified after 1 operation
mov = {0: 15, 1: 0, 2: 3, 3: 4, 4: 5, 5: 6, 6: 2, 7: 8, 8: 7,
       9: 14, 10: 1, 11: 10, 12: 13, 13: 11, 14: 9, 15: 12}

with open("input", "r") as file:
    for k in range(100000):
        for w in p.keys():
            p[w] = mov[p[w]]

print(str(p))

p = {'a': 0, 'b': 1, 'c': 2, 'd': 3, 'e': 4, 'f': 5,
     'g': 6, 'h': 7, 'i': 8, 'j': 9, 'k': 10, 'l': 11,
     'm': 12, 'n': 13, 'o': 14, 'p': 15}

mov1000 = {0: 10, 1: 11, 2: 2, 3: 3, 4: 4, 5: 5, 6: 6, 7: 7, 8: 8,
           9: 9, 10: 13, 11: 12, 12: 0, 13: 15, 14: 14, 15: 1}

with open("input", "r") as file:
    for k in range(int(10000)):
        for w in p.keys():
            p[w] = mov1000[p[w]]


print(str(p))
# sol pacdefghijbknlom
