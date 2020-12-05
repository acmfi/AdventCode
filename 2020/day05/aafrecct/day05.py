def boarding_passes():
    with open('day05.input', 'r') as f:
        while (line := f.readline()) != '':
            line = line.strip('\n')
            row = int(line[:-3].replace('F', '0').replace('B', '1'), 2)
            col = int(line[-3:].replace('L', '0').replace('R', '1'), 2)
            yield row, col


def star1():
    res = 0
    for b_pass in boarding_passes():
        val = b_pass[0] * 8 + b_pass[1]
        res = max(res, val)
    print(b_pass, res)


def star2():
    d = {}
    for i in boarding_passes():
        try:
            d[i[0]] += [i[1]]
        except:
            d[i[0]] = [i[1]]
    for i in d:
        if len(d[i]) < 8:
            print(i, d[i])


star1()
star2()
