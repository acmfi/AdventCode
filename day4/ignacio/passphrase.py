from functools import reduce

with open('input.txt', 'r') as pphrases:
    correct1 = 0
    correct2 = 0 
    for pphrase in pphrases:
        pp = pphrase.split()
        if len(set(pp)) == len(pp):
            correct1 = correct1 + 1
        pps = ["".join(sorted(w)) for w in pp]
        if len(set(pps)) == len(pp):
            correct2 = correct2 + 1
    print("Star 1:", correct1)
    print("Star 2:", correct2)
    
            
    
