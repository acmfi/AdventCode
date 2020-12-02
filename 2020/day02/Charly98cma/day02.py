import re

def star1():
    with open('input.txt', 'r') as f:
        pwdCount = 0
        for line in f:
            # {'1','3','a','abcde'}
            line = re.split('[-|:| ]', line.strip())
            line.remove('')
            pwd = list(line[3])
            # +1 because the top of the range is not included on the range
            if pwd.count(line[2]) in range(int(line[0]), int(line[1])+1):
                pwdCount += 1
        print("Star 1 solution =", pwdCount)


star1()
