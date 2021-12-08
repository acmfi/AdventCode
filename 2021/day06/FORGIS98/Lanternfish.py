def star(stages, days, txt):
    for day in range(days):
        first = stages.pop(0)
        stages.append(first if first != 0 else 0)
        stages[6] += first

    print(txt, sum(stages))


if __name__ == "__main__":
    with open("/home/jorge/input.txt") as file:
        data = list(map(int, file.read().split(',')))

    stages = [0, 0, 0, 0, 0, 0, 0, 0, 0]
    for d in data:
        stages[d] += 1

    second_star_states = stages[:]

    star(stages, 80, "First Star:")
    star(second_star_states, 256, "Second Star:")
