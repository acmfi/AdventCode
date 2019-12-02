data1 = open('imput1.txt', 'r').readlines()

def fuel(data, fun):
    return sum(map(fun, data))

def _rec(a):
    return _rec(x) + x if (x:=int(a)//3-2)>0 else 0

print(f'Solution 1: {fuel(data1, lambda a : int(a)//3-2)}')
print(f'Solution 2: {fuel(data1, _rec)}')
