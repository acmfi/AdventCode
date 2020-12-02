import re

with open('input.txt', 'r') as f:
    pwdCount1 = pwdCount2 = 0
    for line in f:
        # {'1','3','a','abcde'}
        line = re.split('[-|:| ]', line.strip())
        line.remove('')
        pwd = list(line[3])
        # +1 because the top of the range is not included on the range
        if pwd.count(line[2]) in range(int(line[0]), int(line[1])+1):
            pwdCount1 += 1
        # -1 because arrays start at 1 (for some reason :D)
        if (pwd[int(line[0])-1] == line[2]) ^ (pwd[int(line[1])-1] == line[2]):
            pwdCount2 += 1
    print("Star 1 solution =", pwdCount1)
    print("Star 2 solution =", pwdCount2)
