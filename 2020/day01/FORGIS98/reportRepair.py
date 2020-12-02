inputFile = open("input.txt", "r")

lines = inputFile.readlines()
lines = list(map(int, lines))

print("Primera Estrella: ")
for i in range(len(lines)):
    for y in range(++i, len(lines)):
        if(lines[i] + lines[y] == 2020):
            print(lines[i] * lines[y])

print("Segunda Estrella: ")
for i in range(len(lines)):
    for y in range(++i, len(lines)):
        for z in range(++y, len(lines)):
            if(lines[i] + lines[y] + lines[z] == 2020):
                print(lines[i] * lines[y] * lines[z])

inputFile.close()
