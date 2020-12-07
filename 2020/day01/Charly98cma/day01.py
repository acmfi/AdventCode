l = open("input.txt", "r").read().split('\n')[:-1]
found = False
for x in l:
    for y in l:
        if (int(x)+int(y)==2020):
            print("1st STAR SOLUTION ->", int(x)*int(y))
            found = True
            break
    if (found):
        break

found = False
for x in l:
    for y in l:
        for z in l:
            if (int(x)+int(y)+int(z)==2020):
                print("2nd STAR SOLUTION ->", int(x)*int(y)*int(z))
                found = True
                break
        if (found):
            break
    if (found):
        break
