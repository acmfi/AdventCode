#!/usr/bin/env python

inputcode = "L1, L5, R1, R3, L4, L5, R5, R1, L2, L2, L3, R4, L2, R3, R1, L2, R5, R3, L4, R4, L3, R3, R3, L2, R1, L3, R2, L1, R4, L2, R4, L4, R5, L3, R1, R1, L1, L3, L2, R1, R3, R2, L1, R4, L4, R2, L189, L4, R5, R3, L1, R47, R4, R1, R3, L3, L3, L2, R70, L1, R4, R185, R5, L4, L5, R4, L1, L4, R5, L3, R2, R3, L5, L3, R5, L1, R5, L4, R1, R2, L2, L5, L2, R4, L3, R5, R1, L5, L4, L3, R4, L3, L4, L1, L5, L5, R5, L5, L2, L1, L2, L4, L1, L2, R3, R1, R1, L2, L5, R2, L3, L5, L4, L2, L1, L2, R3, L1, L4, R3, R3, L2, R5, L1, L3, L3, L3, L5, R5, R1, R2, L3, L2, R4, R1, R1, R3, R4, R3, L3, R3, L5, R2, L2, R4, R5, L4, L3, L1, L5, L1, R1, R2, L1, R3, R4, R5, R2, R3, L2, L1, L5"


def newdir(ndir, odir):
    if odir == "N":
        return ndir
    elif odir == "R":
        return "U" if ndir == "L" else "D"
    elif odir == "U":
        return "L" if ndir == "L" else "R"
    elif odir == "L":
        return "D" if ndir == "L" else "U"
    elif odir == "D":
        return "R" if ndir == "L" else "L"


def update(datadir, n, direct, hor, ver):
    ndir = newdir(datadir, direct)
    print("NEWDIR: " + ndir)
    n = n if ndir in ["R", "U"] else -n
    if ndir in ["R", "L"]:
        return hor + n, ver, ndir
    else:
        return hor, ver + n, ndir

hor = 0
ver = 0

direct = "N"

for d in inputcode.split(","):
    ds = d.strip()
    print("D: {}, ND: {}".format(direct, ds))
    hor, ver, direct = update(ds[0], int(ds[1:]), direct, hor, ver)
    print("H: {}, V: {}, D: {}".format(hor, ver, direct))

print(abs(hor) + abs(ver))
