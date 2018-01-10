# inpt = open('input', 'r')
inpt = open('day16/rubenaguadoc/input', 'r')
inpt = inpt.read().rstrip('\n').split(',')

abc = 'abcdefghijklmnop'
dancers = [x for x in abc]
states = []

for i in range(1000000000):
    if ''.join(dancers) in states:
        stateIdx = states.index(''.join(dancers))
        dancers = states[stateIdx + (1000000000 - i) % (i - stateIdx)]
        break
    states += [''.join(dancers)]
    for step in inpt:
        if step[0] == 's':
            n = int(step[1:])
            dancers = (dancers + dancers)[16 - n:32 - n:1]
        elif step[0] == 'x':
            p1 = int(step[1:].split('/')[0])
            p2 = int(step[1:].split('/')[1])
            tmp = dancers[p1]
            dancers[p1] = dancers[p2]
            dancers[p2] = tmp
        elif step[0] == 'p':
            p1 = dancers.index(step[1:].split('/')[0])
            p2 = dancers.index(step[1:].split('/')[1])
            tmp = dancers[p1]
            dancers[p1] = dancers[p2]
            dancers[p2] = tmp
    if i == 0:
        print(''.join(dancers))

print(''.join(dancers))
