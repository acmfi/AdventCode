class SSD(object):
    def __init__(self, inpt):
        self.Grid = [[0] * 100 for _ in range(100)]
        self.x = 50
        self.y = 50
        self.inpt = inpt
        self.generate()
        maxs = []
        for i in self.Grid:
            for j in i:
                if j > self.inpt:
                    maxs.append(j)
        print(min(maxs))

    def move(self, direction):
        if direction == 'N':
            self.y += 1
        if direction == 'S':
            self.y -= 1
        if direction == 'E':
            self.x += 1
        if direction == 'W':
            self.x -= 1

    def getValue(self):
        return self.Grid[self.x][self.y]

    def calculateValue(self):
        suma = 0
        circle = ['E', 'N', 'W', 'W', 'S', 'S', 'E', 'E']
        for d in circle:
            self.move(d)
            suma += self.getValue()
        self.move('N')
        self.move('W')
        return suma

    def generate(self):
        self.Grid[self.x][self.y] = 1
        directions = ['E', 'N', 'W', 'S']
        steps = 0
        i = 0
        val = 0
        while val < self.inpt:
            if i % 2 == 0:
                steps += 1
            for z in range(steps):
                self.move(directions[i % 4])
                val = self.calculateValue()
                self.Grid[self.x][self.y] = val
            i += 1
