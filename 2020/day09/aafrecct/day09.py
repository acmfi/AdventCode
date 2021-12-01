from sys import argv
from os import listdir

if len(argv) == 2 and argv[1] in listdir('./'):
    filename = argv[1]
elif 'input' in listdir('./'):
    filename = 'input'
else:
    print('Not a valid input file.')
    exit()


def numbers():
    with open(filename, 'r') as f:
        for i in f:
            yield int(i)


def star1_rule(q, n):
    res = False
    for i in q:
        res |= (n - i) in q
    return res


def star1():
    q = []
    for n in numbers():
        if len(q) < 25:
            q.append(n)
        else:
            if not star1_rule(q, n):
                print(n)
                q.append(n)
                break
            else:
                q.append(n)
                q.pop(0)
    return q[-1]


s1 = star1()


def star2():
    q = []
    for n in numbers():
        q.append(n)
        s = sum(q)
        while s > s1:
            s -= q.pop(0)  
        if s == s1:
            q.sort()
            print(q)
            print(q[0], '+', q[-1], '=', q[0] + q[-1])
            break

star2()
