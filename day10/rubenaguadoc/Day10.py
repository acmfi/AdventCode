rawInpt = '130,126,1,11,140,2,255,207,18,254,246,164,29,104,0,224'


def reverse(idx, pre, chain):
    while idx > 256:
        idx -= 256
    while pre > 256:
        pre -= 256

    if pre <= idx:
        if pre == 0:
            chain = chain[:pre:1] + chain[idx - 1::-1] + chain[idx::1]
        else:
            chain = chain[:pre:1] + chain[idx - 1:pre - 1:-1] + chain[idx::1]
    else:
        tempr = chain[pre::1]
        templ = chain[:idx:1]
        inverse = (tempr + templ)[::-1]
        chain = inverse[len(tempr)::1] + chain[len(templ):len(chain) - len(tempr):1] + inverse[:len(tempr):1]

    return idx, pre, chain


def proccess(inpt):
    chain = [i for i in range(256)]
    idx = 0
    skipSize = 0
    for i in inpt:
        pre = idx
        idx += i
        idx, pre, chain = reverse(idx, pre, chain)
        idx += skipSize
        skipSize += 1
        if skipSize > 256:
            skipSize -= 256
    return chain


def xor(chain, idx):
    res = 0
    for i in range(idx, idx + 16, 1):
        res = res ^ chain[i]
    return res


# Part 1
inpt = rawInpt.split(',')
inpt = [int(x) for x in inpt]
chain = proccess(inpt)
print(chain[0] * chain[1])

# Part 2
# rawInpt = 'AoC 2017'
inpt = [ord(str(x)) for x in rawInpt]
inpt = inpt + [17, 31, 73, 47, 23]
inpt = inpt * 64
chain = proccess(inpt)
chainRes = []
for i in range(0, 256, 16):
    chainRes = chainRes + [xor(chain, i)]
chainRes = [str(hex(x)).replace('0x', '') for x in chainRes]
res = ""
for i, v in enumerate(chainRes):
    while len(chainRes[i]) < 2:
        chainRes[i] = '0' + v
    res += chainRes[i]
print(res)
