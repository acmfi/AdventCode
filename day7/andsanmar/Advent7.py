with open("input", "r") as file:
    subprocesses = []
    parents = []
    for line in file:
        if("->" in line):
            proc = line.split("->")
            subp = proc[1].split(",")
            parents.append(proc[0].split()[0])
            for s in subp:
                subprocesses.append(s.strip())
    print(str(set(parents)-set(subprocesses)))
