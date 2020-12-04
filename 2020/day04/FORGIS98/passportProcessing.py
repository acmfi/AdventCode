import re

myFile = open("input.txt", "r")
lines = myFile.readlines()

def formatea(lines):
    output = []
    myString = ""
    for l in lines:
        if(l == "\n"):
            output.append(myString)
            myString = ""
        else:
            myString += l

    return output

def firstStar(lines):
    lines = formatea(lines)
    count = 0

    for l in lines:
        rx = len(re.findall(r'(?P<byr>byr):|(?P<iyr>iyr):|(?P<eyr>eyr):|(?P<hgt>hgt):|(?P<hcl>hcl):|(?P<ecl>ecl):|(?P<pid>pid):', l))
        if(rx == 7):
            count += 1

    print(count)

def secondStar(lines):
    lines = formatea(lines)
    count = 0

    for l in lines:
        regexPower = len(re.findall(r'(byr):((19[2-9][0-9]|200[0-2]))|(iyr):((20(1[0-9]|20)))|(eyr):((20(2[0-9]|30)))|(hgt):(((1[5-8][0-9]|19[0-3])cm)|((59|6[0-9]|7[0-6])in))|(hcl):#[0-f]{6}|(ecl):(amb|blu|brn|gry|grn|hzl|oth)|(pid):[0-9]{9}[^0-z]', l))
        if(regexPower == 7):
            count += 1

    print(count)

    pass



# La primera funcion siempre comentada,
# sino el archivo ya ha sido leído por firstStar()

firstStar(lines)
secondStar(lines)
