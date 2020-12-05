file = open("input","r")
counter=0
for line in file:
    words = line.split()
    if(len(words)==len(set(words))):
        counter=counter+1
print(counter)
