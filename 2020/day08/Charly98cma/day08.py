import sys

inst = open(sys.argv[1],'r').read().split('\n')
seenInst = []
acc = 0
pointer = 0

while pointer not in seenInst:
    seenInst.append(pointer)
    actualInst = inst[pointer].split(' ')
    if actualInst[0] == "acc": acc += int(actualInst[1])
    elif actualInst[0] == "jmp":
        pointer += int(actualInst[1])
        continue
    elif actualInst[0] == "nop": pass
    pointer += 1

print("1st STAR SOLUTION ->", acc)
