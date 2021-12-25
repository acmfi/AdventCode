from copy import deepcopy

f = open('input')

input = [line.strip('\n') for line in f.readlines()]

instructions = [x.split(' ') for x in input]

A=[1,1,1,1,26,26,1,26,1,26,1,26,26,26]
B=[13,13,10,15,-8,-10,11,-3,14,-4,14,-5,-8,-11]
C=[15,16,4,14,1,5,1,3,3,7,5,13,3,10]

map_pos = {}
aux_stack = []
for idx,a in enumerate(A):
    if a == 1:
        aux_stack.append(idx)
    else:
        map_pos[aux_stack.pop()] = idx

possible_digits = []
for i in range(14):
    possible_digits.append(list(range(1,10)))

def step_decomp(stack:list,w,b,c):
    if b > 0:
        stack.append(w+c)
    else:
        stack.pop()
    return stack

valid_models = []
def get_valid_monads(stack=[],step=0,monad=0):
    if step == 14:
        if len(stack) == 0:
            valid_models.append(monad)
        return
    elif B[step] < 0:
        w = stack[-1]+B[step]
        stack = step_decomp(deepcopy(stack),w,B[step],C[step])
        get_valid_monads(deepcopy(stack),step=step+1,monad=monad*10+w)
        return
    elif B[step] > 9:
        rr = [x for x in range(1,10) if 1<=x+C[step]+B[map_pos[step]]<=9]
        for w in rr:
            stack = step_decomp(deepcopy(stack),w,B[step],C[step])
            get_valid_monads(deepcopy(stack),step=step+1,monad=monad*10+w)
            stack.pop()
        return
        

get_valid_monads()
print(max(valid_models))
print(min(valid_models))