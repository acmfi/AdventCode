from Day18Part2 import main
# inpt = open('input', 'r')
inpt = open('day18/rubenaguadoc/input', 'r')
inpt = inpt.read().rstrip('\n').split('\n')
instructions = [x.split(' ') for x in inpt]
variables = {}


def val(param):
    if param.isalpha():
        if param not in variables.keys():
            variables[param] = 0
        return variables[param]
    else:
        return int(param)


i = 0
while i < len(instructions):
    instruction = instructions[i]
    if instruction[1] not in variables.keys():
        variables[instruction[1]] = 0

    if instruction[0] == 'snd':
        sound = val(instruction[1])
    elif instruction[0] == 'set':
        variables[instruction[1]] = val(instruction[2])
    elif instruction[0] == 'add':
        variables[instruction[1]] += val(instruction[2])
    elif instruction[0] == 'mul':
        variables[instruction[1]] *= val(instruction[2])
    elif instruction[0] == 'mod':
        variables[instruction[1]] %= val(instruction[2])
    elif instruction[0] == 'rcv':
        if not val(instruction[1]) == 0:
            print(sound)
            break
    elif instruction[0] == 'jgz':
        if val(instruction[1]) > 0:
            i += val(instruction[2])
            continue
    i += 1

print(main())
