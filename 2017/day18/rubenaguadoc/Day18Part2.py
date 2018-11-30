# inpt = open('input', 'r')
inpt = open('day18/rubenaguadoc/input', 'r')
inpt = inpt.read().rstrip('\n').split('\n')
instructions = [x.split(' ') for x in inpt]

pv = {'i0': 0, 'i1': 0}
stack = {'variables0': {}, 'variables1': {}}
msgs = {'sender0': [], 'sender1': []}
count = 0


def val(pid, param):
    if param.isalpha():
        if param not in stack['variables' + pid].keys():
            if param == 'p':
                stack['variables' + pid][param] = int(pid)
            else:
                stack['variables' + pid][param] = 0
        return stack['variables' + pid][param]
    else:
        return int(param)


def proccess(pid):
    # print(pid, stack['variables' + pid], msgs['sender' + str((int(pid) + 1) % 2)], pv['i' + pid], instructions[pv['i' + pid]])
    if pv['i' + pid] > len(instructions):
        return True, False
    instruction = instructions[pv['i' + pid]]
    if instruction[1] not in stack['variables' + pid].keys():
        if instruction[1] == 'p':
            stack['variables' + pid][instruction[1]] = int(pid)
        else:
            stack['variables' + pid][instruction[1]] = 0
    if instruction[0] == 'snd':
        msgs['sender' + pid] += [val(pid, instruction[1])]
        if pid == '1':
            global count
            count += 1
    elif instruction[0] == 'set':
        stack['variables' + pid][instruction[1]] = val(pid, instruction[2])
    elif instruction[0] == 'add':
        stack['variables' + pid][instruction[1]] += val(pid, instruction[2])
    elif instruction[0] == 'mul':
        stack['variables' + pid][instruction[1]] *= val(pid, instruction[2])
    elif instruction[0] == 'mod':
        stack['variables' + pid][instruction[1]] %= val(pid, instruction[2])
    elif instruction[0] == 'rcv':
        if msgs['sender' + str((int(pid) + 1) % 2)]:
            stack['variables' + pid][instruction[1]] = msgs['sender' + str((int(pid) + 1) % 2)][0]
            del msgs['sender' + str((int(pid) + 1) % 2)][0]
        else:
            return False, True
    elif instruction[0] == 'jgz':
        if val(pid, instruction[1]) > 0:
            pv['i' + pid] += val(pid, instruction[2])
            return False, False
    pv['i' + pid] += 1
    return False, False


def main():
    terminated0, terminated1, waiting0, waiting1 = False, False, False, False
    while not (terminated0 or terminated1) and not (waiting0 and waiting1):
        terminated0, waiting0 = proccess('0')
        terminated1, waiting1 = proccess('1')

    return count
