import binascii


def solve(l, rnds):
    h = list(range(256))

    n = 0
    s = 0

    for _ in range(rnds):
        for x in l:
            for i in range(x // 2):
                h1 = (n + i) % len(h)
                h2 = (n + x - 1 - i) % len(h)
                y = h[h1]
                h[h1] = h[h2]
                h[h2] = y
            n += x + s
            s += 1

    return h


def knot(l):
    l2 = [ord(n) for n in l] + [17, 31, 73, 47, 23]
    h = solve(l2, 64)

    hexh = ''
    for i in range(len(h) // 16):
        n = 0
        for j in range(16):
            n ^= h[i * 16 + j]
        hexh += f'{n:02x}'

    return hexh


def binary(x):
    return "".join(
        reversed([i+j for i, j in zip(*[["{0:04b}". format(
            int(c, 16)) for c in reversed("0"+x)][n::2] for n in [1, 0]])]))


file = open("result", "w")
# input = "oundnydw"
input = "flqrgnkx"
s = 0
k = []
for x in range(0, 128):
    a = str(bin(int(knot(input+"-"+str(x)), 16)))
    s += a.count("1")
    b = knot(input+"-"+str(x))
    c = binascii.unhexlify(b)
    file.write(binary(b)+"\n")
file.close()
print(s)

nBlocks = 0
# TODO
