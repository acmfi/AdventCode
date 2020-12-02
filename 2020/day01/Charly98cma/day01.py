with open("input.txt", "r") as f:
    l = f.read().split('\n')[:-1]
    print("1st star -> " +
          str([int(x)*int(y) for x in l for y in l if int(x)+int(y)==2020][0]))
    print("2nd star -> " +
          str([int(x)*int(y)*int(z) for x in l for y in l for z in l if int(x)+int(y)+int(z)==2020][0]))
