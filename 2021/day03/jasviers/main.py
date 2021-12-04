def gamma(list):
    return int(most_common(list), 2)

def epsilon(list):
    return int(most_common(list, "reversed"), 2)

def most_common(list, type_common=None):
    countlist = [0 for _ in range(len(list[0]))]

    for i in list:
        countlist = [i + j for i, j in zip(countlist, map(int, i.strip()))]

    if type_common == "reversed":
        return "".join(["0" if i  >= (len(list)/2) else "1" for i in countlist])

    return "".join(["1" if i  >= (len(list)/2) else "0" for i in countlist])

def oxygen(list):
    for i in range(len(list[0])):
        if len(list) == 1:
            break
        common = most_common(list)
        list = [num for num in list if num[i] == common[i]]

    return int(list[0], 2)

def co2(list):
    for i in range(len(list[0])):
        if len(list) == 1:
            break
        common = most_common(list, "reversed")
        list = [num for num in list if num[i] == common[i]]
    
    return int(list[0], 2)

if __name__ == "__main__":

    with open("./input", "r") as fd:
        list = fd.readlines()

    print("First:", gamma(list)*epsilon(list))
    print("Second:", oxygen(list)*co2(list))