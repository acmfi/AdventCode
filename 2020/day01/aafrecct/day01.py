mod_classes = {i:[] for i in range(10)}


def next_number():
    with open('day1.input', 'r') as f:
        while (l := f.readline()) != '':
            yield int(l)


def star1():
   for i in next_number():
       mod_i = i % 10
       for j in mod_classes[(10-mod_i) % 10]:
           if (i + j == 2020):
               print('STAR 1 numbers are: ', i, 'and', j)
               print('Solution:', i * j)
               return
       else:
           mod_classes[mod_i].append(i)


def star2():
   for i in next_number():
       for j in next_number():
           if j < 2020 - i:
               for k in next_number():
                   if i + j + k == 2020:
                       print ('STAR 2 numbers are: ', i, ',', j, 'and', k)
                       print ('Solution:', i * j * k)
                       return

star1()
star2()
