from itertools import product

f = open('input')

input = [line.strip('\n') for line in f.readlines()]

p1 = int(input[0].split(' ')[-1])
p2 = int(input[1].split(' ')[-1])

p1_score = 0
p2_score = 0

dice = list(range(1,101))
idx = 0
finished = False
player_1 = True
dice_rolled = 0
while not finished:
    dice_rolled += 3
    res = 0
    for _ in range(3):
        res += dice[idx]
        idx = idx + 1 if idx <99 else 0
    if player_1:
        p1_score += (p1 + res) % 10 if (p1 + res) % 10 != 0 else 10
        p1 = (p1 + res) % 10 if (p1 + res) % 10 != 0 else 10
        if p1_score >= 1000:
            finished = True
    else:
        p2_score += (p2 + res) % 10 if (p2 + res) % 10 != 0 else 10
        p2 = (p2 + res) % 10 if (p2 + res) % 10 != 0 else 10
        if p2_score >= 1000:
            finished = True
    player_1 = not player_1
print(min(p1_score,p2_score)*dice_rolled)

# Star 2: Quantum dice

p1 = int(input[0].split(' ')[-1])
p2 = int(input[1].split(' ')[-1])

movements = {}
def check_universe(p1,p2,p1_score,p2_score):
    if p1_score > 20:
        return (1,0)
    if p2_score > 20:
        return (0,1)
    if (p1,p2,p1_score,p2_score) in movements:
        return movements[(p1,p2,p1_score,p2_score)]
    ans = (0,0)
    for d1 in range(1,4):
        for d2 in range(1,4):
            for d3 in range(1,4):
                scr = d1+d2+d3
                n_p1_score = p1_score + ((p1 + scr) % 10 if (p1 + scr) % 10 != 0 else 10)
                n_p1 = (p1 + scr) % 10 if (p1 + scr) % 10 != 0 else 10
                w1,w2 = check_universe(p2,n_p1,p2_score,n_p1_score) # Switch positions between Player 1 and 2
                ans = (ans[0]+w2,ans[1]+w1) # Because the positions were switched, we also have to switch the tuple values when adding
    movements[(p1,p2,p1_score,p2_score)] = ans
    return ans

print(max(check_universe(p1,p2,0,0)))