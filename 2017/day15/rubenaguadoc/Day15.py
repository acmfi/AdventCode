print('It may take a while... (Up to 100\')')
inptA = 883
inptB = 879
# Example
# inptA = 65
# inptB = 8921

pA = inptA
pB = inptB
factorA = 16807
factorB = 48271
div = 2147483647
count = 0

for i in range(40000000):
    pA = (pA * factorA) % div
    pB = (pB * factorB) % div
    if bin(pA)[-16:] == bin(pB)[-16:]:
        count += 1

print(count)

pA = inptA
pB = inptB
count = 0

for i in range(5000000):
    pA = (pA * factorA) % div
    pB = (pB * factorB) % div
    while not pA % 4 == 0:
        pA = (pA * factorA) % div
    while not pB % 8 == 0:
        pB = (pB * factorB) % div
    if bin(pA)[-16:] == bin(pB)[-16:]:
        count += 1

print(count)
