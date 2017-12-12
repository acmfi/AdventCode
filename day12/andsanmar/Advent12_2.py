relations = {}
with open("input", "r") as file:
    for line in file:
        p = line.split("<->")
        s = p[1].split(",")
        right = []
        for sub in s:
            right.append(int(sub))
        k = p[0]
        relations[int(p[0])] = right
    length = 1
    newlength = 0
    subprocesses = [0]
    gr = 1
    for z in relations.keys():
        if z not in subprocesses:
            gr = gr + 1
            subprocesses.append(z)
            length = 1
            newlength = 0
        while length != newlength:
            length = len(subprocesses)
            for x in subprocesses:
                for y in relations[x]:
                    if y not in subprocesses:
                        subprocesses.append(y)
            newlength = len(subprocesses)

print(gr)
