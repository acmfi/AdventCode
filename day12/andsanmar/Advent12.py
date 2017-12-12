relations = {}
with open("input_sample", "r") as file:
    for line in file:
        p = line.split("<->")
        s = p[1].split(",")
        right = []
        for sub in s:
            right.append(int(sub))
        k = p[0]
        relations[int(p[0])] = right
#    print(str(relations))
    length = 1
    newlength = 0
    subprocesses = [0]
    while length != newlength:
        length = len(subprocesses)
        for x in subprocesses:
            for y in relations[x]:
                if y not in subprocesses:
                    subprocesses.append(y)
        newlength = len(subprocesses)

print(length)
