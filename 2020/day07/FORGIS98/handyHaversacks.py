import sys
import re

MYBAG = "shiny gold"
yaContados = []
puntero = 0

for firstPointer in reversed(list(open(sys.argv[1]))):
    for secondPointer in reversed(list(open(sys.argv[1]))):
        secondPointer = secondPointer.rstrip()

        rx = re.match(r'.* bags contain (?P<otherBag>.*)', secondPointer)
        bigBag = re.match(r'(?P<bag>.*) bags contain', secondPointer)

        if(MYBAG in rx.group("otherBag") and not (bigBag.group("bag") in yaContados)):
            yaContados.append(bigBag.group("bag"))

    if(puntero < len(yaContados)):
        MYBAG = yaContados[puntero]
        puntero += 1
    else:
        break

print(len(yaContados))
