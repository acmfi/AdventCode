file = open('pass', 'r')
inpt = file.read()

inpt = inpt.splitlines()
count = 0
for val in inpt:
    words = val.split()
    repe = False
    for indx, word in enumerate(words):
        for indx2, word2 in enumerate(words):
            if(word == word2 and not indx == indx2):
                repe = True
    if not repe:
        count += 1
print(count)

count = 0
for val in inpt:
    words = val.split()
    repe = False
    for indx, word in enumerate(words):
        for indx2, word2 in enumerate(words):
            word = list(word)
            word2 = list(word2)
            word.sort()
            word2.sort()
            if(word == word2 and not indx == indx2):
                repe = True
    if not repe:
        count += 1
print(count)
