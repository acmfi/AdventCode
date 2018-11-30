with open("input", "r") as file:
    s = 0
    s2 = 0
    for line in file:
        numbers = list(map(lambda x: int(x), line.split()))
        s = s + max(numbers) - min(numbers)
        for x in numbers:
            for y in numbers:
                if(x != y and (x % y == 0 or y % x == 0)):
                    if(x > y):
                        s2 = s2 + x / y
                    else:
                        s2 = s2 + y / x
print(s)
print(s2/2)
