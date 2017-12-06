file = open("input","r")
counter=0
for line in file:
    words = line.split()
    listOfWords = []
    finded = False
    for word in words:
        if(not set(word) in listOfWords and not finded):
            listOfWords.append(set(word))
        else:
            finded = True
    if not finded:
        counter = counter + 1
print(counter)
