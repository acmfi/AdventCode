with open("input", "r") as file:
    r = {}
    for line in file:
        a = line.split(":")
        r[int(a[0])] = int(a[1])
    s = 0
    for x in r.keys():
        if(x % (2*(r[x]-1)) == 0):
            s = x*r[x] + s
    print(s)
    f = False
    c = 0
    while(not f):
        f = True
        for x in r.keys():
            if((c + x) % (2*(r[x]-1)) == 0):
                f = False
        if(f):
            print(c)
        c = c + 1
