def distance(dir):
    path1 = dir["n"]-dir["s"]
    path2 = dir["ne"]-dir["sw"]
    path3 = dir["nw"]-dir["se"]
    return (path1-path3 + path2 + path3)


distances = []
dirs = {"n": 0, "ne": 0, "se": 0, "s": 0, "sw": 0, "nw": 0}
dirs2 = {"n": 0, "ne": 0, "se": 0, "s": 0, "sw": 0, "nw": 0}
with open("input", "r") as file:
    for line in file:
        paths = line.split(",")
        for p in paths:
            dirs[p] = dirs[p] + 1
            if(p == "n"):
                if(dirs2["s"] == 0):
                    dirs2[p] = dirs2[p] + 1
                else:
                    dirs2["s"] = dirs2["s"] - 1
            elif(p == "ne"):
                if(dirs2["sw"] == 0):
                    dirs2[p] = dirs2[p] + 1
                else:
                    dirs2["sw"] = dirs2["sw"] - 1
            elif(p == "nw"):
                if(dirs2["se"] == 0):
                    dirs2[p] = dirs2[p] + 1
                else:
                    dirs2["se"] = dirs2["se"] - 1
            elif(p == "se"):
                if(dirs2["nw"] == 0):
                    dirs2[p] = dirs2[p] + 1
                else:
                    dirs2["nw"] = dirs2["nw"] - 1
            elif(p == "sw"):
                if(dirs2["ne"] == 0):
                    dirs2[p] = dirs2[p] + 1
                else:
                    dirs2["ne"] = dirs2["ne"] - 1
            elif(p == "s"):
                if(dirs2["n"] == 0):
                    dirs2[p] = dirs2[p] + 1
                else:
                    dirs2["n"] = dirs2["n"] - 1
            distances.append(distance(dirs))

print(distance(dirs))
print(max(distances))
