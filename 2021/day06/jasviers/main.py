def lanternfish(data, days):
    fishes = [data.count(i) for i in range(9)]
    for _ in range(days):
        fishes[7] += fishes[0]
        fishes = fishes[1:] + fishes[:1]
    return sum(fishes)

if __name__ == "__main__":

    with open("./input", "r") as fd:
        data = fd.read()

    data = list(map(int, data.split(",")))

    print("First:", lanternfish(data, 80))
    print("Second:", lanternfish(data, 256))