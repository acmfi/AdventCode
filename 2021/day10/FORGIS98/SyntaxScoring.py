def firstStar(lines):
    dic = {
        "}": "{",
        ")": "(",
        "]": "[",
        ">": "<"
    }

    points_table = {
        ")": 3,
        "]": 57,
        "}": 1197,
        ">": 25137
    }

    fake_stack = []
    points = 0

    for line in lines:
        for l in line:
            if (l in "({[<"):
                fake_stack.append(l)
            elif (len(fake_stack) != 0 and fake_stack[len(fake_stack) - 1] == dic[l]):
                fake_stack.pop()
            else:
                points += points_table[l]
                break

    print("First Star:", points)


def secondStar(lines):
    dic = {
        "}": "{",
        ")": "(",
        "]": "[",
        ">": "<"
    }

    points_table = {
        "(": 1,
        "[": 2,
        "{": 3,
        "<": 4
    }

    winners = []
    for line in lines:
        fake_stack = []
        do = True

        for l in line:
            if (l in "({[<"):
                fake_stack.append(l)
            elif (len(fake_stack) != 0 and fake_stack[len(fake_stack) - 1] == dic[l]):
                fake_stack.pop()
            else:
                do = False
                break

        if(do):
            points = 0

            for i in range(1, len(fake_stack) + 1):
                points *= 5
                points += points_table[fake_stack[-i]]

            winners.append(points)

    winners.sort()
    print("Second Star:", winners[int(len(winners)/2)])


if __name__ == "__main__":
    with open("/home/jorge/input.txt") as file:
        lines = file.readlines()
        lines = [line.rstrip() for line in lines]

    firstStar(lines)
    secondStar(lines)
