file = open('jumpStack', 'r')
rawInpt = file.read()

rawInpt = rawInpt.splitlines()
preIndx = 0
indx = 0
count = 0
inpt = [int(x) for x in rawInpt]
while(indx < len(inpt) and indx >= 0):
    count += 1
    indx += inpt[indx]
    inpt[preIndx] += 1
    preIndx = indx

print(count)


preIndx = 0
indx = 0
count = 0
inpt = [int(x) for x in rawInpt]
while(indx < len(inpt) and indx >= 0):
    count += 1
    indx += inpt[indx]
    if(inpt[preIndx] < 3):
        inpt[preIndx] += 1
    else:
        inpt[preIndx] -= 1
    preIndx = indx

print(count)
