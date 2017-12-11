file = open('registers', 'r')
# file = open('day8/rubenaguadoc/registers', 'r')
inpt = file.read()
inpt = inpt.splitlines()
registers = []
operands = []
vals = []
conditionsP1 = []
conditionsP2 = []
conditionsP3 = []
maxMem = []

for value in inpt:
    value = value.split(' ')
    registers.append(value[0])
    if(value[1] == 'inc'):
        operands.append('+')
    else:
        operands.append('-')
    vals.append(value[2])
    conditionsP1.append(value[4])
    conditionsP2.append(value[5])
    conditionsP3.append(value[6])

SingleRegisters = list(set(registers))
SingleValues = [0] * len(SingleRegisters)

for idx, val in enumerate(registers):
    regPos = SingleRegisters.index(conditionsP1[idx])
    condition = str(SingleValues[regPos]) + conditionsP2[idx] + str(conditionsP3[idx])
    if(eval(condition)):
        operation = str(SingleValues[SingleRegisters.index(val)]) + operands[idx] + str(vals[idx])
        SingleValues[SingleRegisters.index(val)] = eval(operation)
        maxMem.append(max(SingleValues))

print(max(SingleValues))
print(max(maxMem))
