import copy

f = open('input.txt')
instructions = list(map(lambda x: x.split(' '),f.read().split('\n')))

accum = 0
idx = 0
visited_ins = []

def sum(arg):
    global accum
    global idx
    accum += arg
    idx += 1

def jmp(arg):
    global idx
    idx += arg

def nop(arg):
    global idx
    idx += 1

switcher = {
        'acc' : sum,
        'jmp' : jmp,
        'nop' : nop
    }
def find_loop(ins):
    global idx
    global accum
    while idx < len(ins):
        action = ins[idx][0]
        arg = int(ins[idx][1])
        visited_ins.append(idx)
        switcher[action](arg)
        if idx in visited_ins:
            break
    return accum

print('First star solution:',find_loop(instructions))

visited_ins = []
idx = 0
accum = 0
search_idx = 0

while search_idx < len(instructions):
    alt_instuctions = copy.deepcopy(instructions)
    inst = alt_instuctions[search_idx][0]
    if inst != 'acc':
        if inst == 'jmp':
            alt_instuctions[search_idx][0] = 'nop'
        else:
            alt_instuctions[search_idx][0] = 'jmp'
        find_loop(alt_instuctions)
        if idx < len(alt_instuctions):
            idx = 0
            accum = 0
            search_idx += 1
            visited_ins = []
        else:
            break
    else:
        search_idx += 1

print('Second star solution:',accum)