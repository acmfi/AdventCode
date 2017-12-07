inpt = 325489

currentPointer = 1
row = -1
counter = -1

while currentPointer < inpt:
    row += 2
    currentPointer = row**2
    counter += 1

esquinaDist = row - 1
esq1 = currentPointer
esq2 = currentPointer - esquinaDist
esq3 = currentPointer - esquinaDist * 2
esq4 = currentPointer - esquinaDist * 3
esq5 = currentPointer - esquinaDist * 4

d1 = esq1 - inpt
d2 = esq2 - inpt
d3 = esq3 - inpt
d4 = esq4 - inpt
d5 = esq5 - inpt
dist = min([abs(x) for x in [d1, d2, d3, d4, d5]])

print(2 * counter - dist)
