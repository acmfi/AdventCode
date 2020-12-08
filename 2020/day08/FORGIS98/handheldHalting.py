import re
import sys

myFile = open(sys.argv[1], "r")
lines = myFile.readlines()

lines = list(map(lambda x: x.rstrip(), lines))

accumulator = 0
executed = []
PC = 0

while PC not in executed:
    rx = re.match(r'(?P<op>\w+) (?P<val>[+-]\d+)', lines[PC])

    if(rx.group("op") == "jmp"):
        executed.append(PC)
        PC += int(rx.group("val"))
    elif(rx.group("op") == "acc"):
        executed.append(PC)
        accumulator += int(rx.group("val"))
        PC += 1
    else:
        executed.append(PC)
        PC += 1

print(accumulator)
