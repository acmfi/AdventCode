with open("input", "r") as file:
    registers = {}
    for line in file:
        registers[line.split()[0]] = 0
    # Registers initialized

with open("input", "r") as file:
    for line in file:
        elem = line.split()
        result = {
            '==': lambda x, y: x == y,
            '<': lambda x, y: x < y,
            '>': lambda x, y: x > y,
            '<=': lambda x, y: x <= y,
            '>=': lambda x, y: x >= y,
            '!=': lambda x, y: x != y,
        }[elem(5)](registers[elem[4]], int(elem[6]))
        if result:
            if elem[1] == 'dec':
                registers[elem[0]] = registers[
                    elem[0]] - int(registers[elem[2]])
            else:
                registers[elem[0]] = registers[
                    elem[0]] + int(registers[elem[2]])
    print(str(registers))
