def next_line():
    with open('day02.input', 'r') as f:
        while(l := f.readline()) != '':
            yield l

def parse_line(line):
    line = line.split(' ')
    line[0] = [int(i) for i in line[0].split('-')]
    line[1] = line[1].replace(':', '')
    line[2] = line[2].replace('\n', '')
    return line
    

def check_star_1(parsed_line):
    return parsed_line[0][0] <= parsed_line[2].count(parsed_line[1]) <= parsed_line[0][1]

def check_star_2(parsed_line):
    a = 0
    for i in range(2):
        if parsed_line[0][i] <= len(parsed_line[2]):
            a += parsed_line[2][parsed_line[0][i] - 1] == parsed_line[1] 
    return a == 1 

star1 = 0
star2 = 0
for l in next_line():
    l = parse_line(l)
    star1 += 1 if check_star_1(l) else 0
    star2 += 1 if check_star_2(l) else 0
print(star1)
print(star2)

