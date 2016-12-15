import re

puzzle_input = "R5, R4, R2, L3, R1, R1, L4, L5, R3, L1, L1, R4, L2, R1, R4, R4, L2, L2, R4, L4, R1, R3, L3, L1, L2, R1, R5, L5, L1, L1, R3, R5, L1, R4, L5, R5, R1, L185, R4, L1, R51, R3, L2, R78, R1, L4, R188, R1, L5, R5, R2, R3, L5, R3, R4, L1, R2, R2, L4, L4, L5, R5, R4, L4, R2, L5, R2, L1, L4, R4, L4, R2, L3, L4, R2, L3, R3, R2, L2, L3, R4, R3, R1, L4, L2, L5, R4, R4, L1, R1, L5, L1, R3, R1, L2, R1, R1, R3, L4, L1, L3, R2, R4, R2, L2, R1, L5, R3, L3, R3, L1, R4, L3, L3, R4, L2, L1, L3, R2, R3, L2, L1, R4, L3, L5, L2, L4, R1, L4, L4, R3, R5, L4, L1, L1, R4, L2, R5, R1, R1, R2, R1, R5, L1, L3, L5, R2"

direction = re.compile('([LR])([0-9]*)')


def turn(to, point):
    if (to == 'L'):
        point = (point - 1) % len(coords)
    elif (to == 'R'):
        point = (point + 1) % len(coords)
    return point

def walk(steps, point, moves):
    moves[point] += steps
    return moves

def moves_to_coords(moves):
    p = (moves[0]-moves[2],moves[1]-moves[3])
    return "{}N{}E".format(p[0],p[1])

def walk_every(steps, point, moves):
    result = set()
    for i in range(1,steps+1):
        moves[point] += 1
        result.add(moves_to_coords(moves))
    return moves, result

def places_visited(orig, dest):
    # Orig: [N, E, S, O]  # Dest: [N, E, S, O]
    p_ini = (orig[0]-orig[2],orig[1]-orig[3])
    p_fin = (dest[0]-dest[2],dest[1]-dest[3])
    difx = p_ini[0]-p_fin[0]
    dify = p_ini[1]-p_fin[1]
    result=[]
    for i in range(0,difx):
        print(i)
        result.add('({},{})'.format(p_ini[0]+i,p_ini[1]))
    for i in range(0,dify):
        print(i)
        result.add('({},{})'.format(p_ini[0],p_ini[1]+i))
    return result
    
    # TODO

coords = ['N','E','S','O']
moves =  [ 0 , 0 , 0 , 0 ]
point = 0

instructions = puzzle_input.strip('\n').strip(' ').split(', ')

# Part I

for i in instructions:
    match = re.match(direction,i)
    toLR = match.group(1)
    steps = int(match.group(2))
    point = turn(toLR,point)
    moves = walk(steps,point,moves)

result = abs(moves[0]-moves[2]) + abs(moves[1]-moves[3])
    
print('Part I:\nYou are {} blocks away.'.format(result))    

# Part II
print("Part II")
history = set()
point = 1
moves = [0,0,0,0]
history.add(str(moves))
for i in instructions:
    match = re.match(direction,i)
    toLR = match.group(1)
    steps = int(match.group(2))
    point = turn(toLR,point)
    moves, positions = walk_every(steps,point,moves)
    for pos in positions:
        if pos in history:
            print("You where already in {}".format(pos))
        else:
            history.add(pos)
