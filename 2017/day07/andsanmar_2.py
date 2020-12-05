from functools import reduce


def getWeight(process, weights, relations):
    if process in relations.keys():
        return(weights[process] + reduce(lambda x, y: x + y,
                                         map(lambda x:
                                             getWeight(x,
                                                       weights,
                                                       relations),
                                             relations[process])))
    return(weights[process])


def proveWeight(process, weights, relations):
    if process in relations.keys():
        for x in relations[process]:
            return checkEqual(list(map(lambda x:
                                       getWeight(x, weights, relations),
                                       relations[process])))
    return(True)


def checkEqual(lst):
    return lst[1:] == lst[:-1]


with open("input", "r") as file:
    weights = {}
    subprocesses = []
    relations = {}
    for line in file:
        if("->" in line):
            proc = line.split("->")
            subp = proc[1].split(",")
            weights[proc[0].split()[0]] = int(proc[0].split()[1].
                                              replace("(", "").
                                              replace(")", ""))
            relations[proc[0].split()[0]] = list(map(
                lambda x: x.strip(), subp))
            for s in subp:
                subprocesses.append(s.strip())
        else:
            weights[line.split()[0]] = int(line.split()[1].
                                           replace("(", "").replace(")", ""))
    for x in weights.keys():
        print(x, getWeight(x, weights, relations),
              proveWeight(x, weights, relations))
# Over the exit we see the process marked with false with the less weight,
#  we focus on it branch, and see wich of the subprocess (which are marked
#  with true) is different weighteded, then we substract the difference
#  of weight of the rest of processes of the same branch
