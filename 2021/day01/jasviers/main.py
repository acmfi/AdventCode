def increased_count(list):
    acc = 0
    last = list[0]

    for elem in list[1::]:
        if elem > last:
            acc += 1
        last = elem
    return acc

def sliding(list):
    i = 0
    elems = []
    while i < len(list)-2:
        elems.append(sum([list[i], list[i+1], list[i+2]]))
        i += 1
    return elems

if __name__ == "__main__":
    with open("./input", "r") as fd:
        list = list(map(int, fd.readlines()))
    
    print("First:", increased_count(list))
    print("Second:", increased_count(sliding(list)))