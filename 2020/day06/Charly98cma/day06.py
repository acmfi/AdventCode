import sys

with open(sys.argv[1],'r') as myFile:
    count1st = 0
    count2nd = 0
    group = dict()
    numLines = 0
    for l in myFile:
        l = l.strip()
        if not l:
            # Empty line
            count1st += len(group)
            # Key is not necessary
            for key,value in group.items():
                if value == numLines:
                    count2nd += 1
            # Reset values for next group of answers
            group = dict()
            numLines = 0
        else:
            # Not empty line
            numLines += 1
            for ans in list(l):
                if ans in group:
                    group[ans] += 1
                else:
                    group[ans] = 1
            pass
    print("1st STAR SOLUTION ->", count1st)
    print("2nd STAR SOLUTION ->", count2nd)
