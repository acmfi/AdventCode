myFile = open("input.txt", "r");
lines = myFile.readlines();
tuplaRow = (0, 127);
tuplaSeat = (0, 7);

def rowRecursivo(line, bottom, top):
    letra = line[:1];
    line = line[1:];

    if((top - bottom) == 1):
        if(letra == 'F'):
            return bottom;
        else:
            return top;
    if(letra == 'F'):
        top = (top+bottom) // 2;
    elif(letra == 'B'):
        if(top % 2 == 0):
            bottom = (top+bottom) // 2;
        else:
            bottom = (top+bottom+1) // 2;

    return rowRecursivo(line, bottom, top);

def seatRecursivo(line, bottom, top):
    letra = line[:1];
    line = line[1:];

    if((top - bottom) == 1):
        if(letra == 'L'):
            return bottom;
        else:
            return top;
    if(letra == 'L'):
        top = (top+bottom) // 2;
    elif(letra == 'R'):
        if(top % 2 == 0):
            bottom = (top+bottom) // 2;
        else:
            bottom = (top+bottom+1) // 2;

    return seatRecursivo(line, bottom, top);

def firstStar(lines):
    res = 0;
    for i in lines:
        row = rowRecursivo(i, 0, 127);
        seat = seatRecursivo(i[7:], 0, 7);
        if((row*8+seat) >= res):
            res = row*8+seat;

    print("First ID: ", res)


def secondStar(lines):
    res = 0;
    lista = [];
    for i in lines:
        row = rowRecursivo(i, 0, 127);
        seat = seatRecursivo(i[7:], 0, 7);
        lista.append(row*8+seat);

    lista.sort()
    for i in range(0, len(lista)-1):
        prev = lista[i]+1;
        if(not(prev == lista[i+1])):
            print("Second ID: ", (lista[i]+1));
            break;

firstStar(lines);
secondStar(lines);
