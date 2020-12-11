import sys, copy

def solve(inst, seenInst):
    try:
        acc = 0
        pointer = 0
        while pointer not in seenInst:
            seenInst.append(pointer)
            if inst[pointer][0] == "acc":
                acc += inst[pointer][1]
            elif inst[pointer][0] == "jmp":
                pointer += inst[pointer][1]
                continue
            elif inst[pointer][0] == "nop":
                pass
            else:
                print("Instruction not supported ->", inst[pointer][0])
                sys.exit(1)
            pointer += 1
        return acc, seenInst
    except IndexError:
        # Error exit used only during the second star solution
        return acc, pointer


# Read the whole file, split each instruction and discard the last empty element
inst = [a.split(' ') for a in open(sys.argv[1],'r').read().split('\n')[:-1]]
# Turn the number on each instruction to integers
for x in inst:
    x[1] = int(x[1])

# Format of inst -> [["instruction1", value1], ["instruction2", value2], ...]

acc, seenInst = solve(inst, [])
print("1st STAR SOLUTION ->", acc)

# Replace each jmp/nop with its counterpart
for i in seenInst:
    inst_bkp = copy.deepcopy(inst)
    if inst[i][0] == "jmp":
        inst_bkp[i][0] = "nop"
    elif inst[i][0] == "nop":
        inst_bkp[i][0] = "jmp"
    else:
        continue
    # Try to solve the puzzle with the replaced instruction
    acc, pointer = solve(inst_bkp, [])
    # If the last instruction executed is the last instruction
    # on the program, that's the solution
    if (pointer == len(inst)):
        print("2nd STAR SOLUTION ->", acc)
        break
