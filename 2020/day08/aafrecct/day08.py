from sys import argv
from os import listdir

if len(argv) == 2 and argv[1] in listdir('./'):
    filename = argv[1]
elif 'input' in listdir('./'):
    filename = 'input'
else:
    print('Not a valid input file.')
    exit()


with open(filename, 'r') as f:
    program = [((i := instruction.split(' '))[0], int(i[1])) for instruction in f.readlines()]


def execute(switch_instruction = -1, capture_key_insts = False):
    r = []
    l = []
    i = 0
    a = 0
    while i not in r and i <= 628:
        op, n = program[i]
        r.append(i)
        if i == switch_instruction:
            if op == 'jmp':
                op = 'nop'
            elif op == 'nop':
                op = 'jmp'
        if op == 'acc':
            a += n
            i += 1
        elif op == 'jmp':
            if capture_key_insts:
                l.append(i)
            i += n
        elif op == 'nop':
            if capture_key_insts:
                l.append(i)
            i += 1
    if capture_key_insts:
        return (i, a, l)
    else:
        return (i, a)

star1 = execute(capture_key_insts = True)

print(star1)

for inst in star1[2]:
    f = execute(switch_instruction=inst)
    if f[0] >= 629:
        print(inst, f)
