import re

myFile = open("input.txt", "r")

def firstStar(myFile):
    count = 0
    for line in myFile:
        rx = re.match(r'^(?P<min>\d+)-(?P<max>\d+) (?P<char>\w): (?P<pass>\d+|\w+)$', line)
    
        regexPower = '^(?:[^' + rx.group('char') + '\s]*' + rx.group('char') + '){' + rx.group('min') + ',' + rx.group('max') + '}[^' + rx.group('char') + '\s]*$'
    
        if(re.match(regexPower, rx.group("pass"))):
            count += 1

    print("Primera Estrella")
    print(count)

def secondStar(myFile):
    count = 0
    for line in myFile:
        rx = re.match(r'^(?P<min>\d+)-(?P<max>\d+) (?P<char>\w): (?P<pass>\d+|\w+)$', line)
        
        first = rx.group("pass")[int(rx.group("min")) - 1] == rx.group("char")
        second = rx.group("pass")[int(rx.group("max")) - 1] == rx.group("char")

        if((first or second) and first != second):
            count += 1

    print("Segunda Estrella")
    print(count)


# La primera funcion siempre comentada,
# sino el archivo ya ha sido leído por firstStar()

# firstStar(myFile)
secondStar(myFile)
