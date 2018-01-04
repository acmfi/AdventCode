from knotHash import knotHash

inpt = 'ljoxqyyw'
# inpt = 'flqrgnkx'
numberOnes = 0
columns = []
for i in range(128):
    kHash = knotHash(inpt + '-' + str(i))
    kHashB = ''
    for l in kHash:
        lBin = bin(int(l, 16))[2:]
        while len(lBin) < 4:
            lBin = '0' + lBin
        kHashB += lBin
    columns += [kHashB]
    numberOnes += kHashB.count('1')

print(numberOnes)

def fixTable(count, colums, valRow, valCol):
    high = max(valRow, valCol)
    low = min(valRow, valCol)
    for rowIdx, rowVal in enumerate(columns):
        for colIdx, colVal in enumerate(rowVal):
            if colVal == 0:
                continue
            if colVal > high:
                columns[rowIdx][colIdx] = colVal - 1
            elif colVal == high:
                columns[rowIdx][colIdx] = low

for idx, val in enumerate(columns):
    columns[idx] = [int(x) for x in val]

if columns[0][0] == 0:
    count = 1
else:
    count = 2

for rowIdx, rowVal in enumerate(columns):
    for colIdx, colVal in enumerate(rowVal):
        if colVal == 0:
            continue
        if rowIdx == 0:
            if not colIdx == 0:
                if columns[0][colIdx - 1] == 0:
                    columns[0][colIdx] = count
                    count += 1
                else:
                    columns[0][colIdx] = columns[0][colIdx - 1]
        else:
            if colIdx == 0:
                if columns[rowIdx - 1][colIdx] == 0:
                    columns[rowIdx][colIdx] = count
                    count += 1
                else:
                    columns[rowIdx][colIdx] = columns[rowIdx - 1][colIdx]
            else:
                if columns[rowIdx - 1][colIdx] == 0 and columns[rowIdx][colIdx - 1] == 0:
                    columns[rowIdx][colIdx] = count
                    count += 1
                elif not columns[rowIdx - 1][colIdx] == 0 and columns[rowIdx][colIdx - 1] == 0:
                    columns[rowIdx][colIdx] = columns[rowIdx - 1][colIdx]
                elif columns[rowIdx - 1][colIdx] == 0 and not columns[rowIdx][colIdx - 1] == 0:
                    columns[rowIdx][colIdx] = columns[rowIdx][colIdx - 1]
                else:
                    columns[rowIdx][colIdx] = min(columns[rowIdx - 1][colIdx], columns[rowIdx][colIdx - 1])
                    if not columns[rowIdx - 1][colIdx] == columns[rowIdx][colIdx - 1]:
                        fixTable(count, columns, columns[rowIdx - 1][colIdx], columns[rowIdx][colIdx - 1])
                        count -= 1


def printGrid(columns):
    for rowIdx, rowVal in enumerate(columns):
        for colIdx, colVal in enumerate(rowVal):
            print(colVal, end='\t')
        print('\n')

# printGrid(columns)

for rowIdx, rowVal in enumerate(columns):
    if count in rowVal:
        print(count)
        exit()

print(count - 1)
