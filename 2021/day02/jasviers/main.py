position = [0, 0, 0]

def forward(n):
    position[0] += n
    if position[1] > 0:
        position[2] += n * position[1]

def up(n):
    position[1] -= n

def down(n):
    position[1] += n

def depth(list):
    for e in list:
        e = e.split()
        eval(f"{e[0]}({e[1]})")
    return position[0] * position[1]

def aim(list):
    for e in list:
        e = e.split()
        eval(f"{e[0]}({e[1]})")
    return position[0] * position[2]

if __name__ == "__main__":
    with open("./input", "r") as fd:
        list = fd.readlines()
    
    #print("First:", depth(list))
    print("Second:", aim(list))