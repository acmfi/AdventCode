import sys

from typing import List, Tuple, Callable

def star1(diags: List[str]) -> int:
    gamma = "".join("1" if n.count("1") > len(diags) / 2 else "0" for n in zip(*diags))
    return int(gamma, 2) * int(gamma.translate(str.maketrans("01", "10")), 2)


def lsr(diags: List[str], pref_bit: str):
    for bit in range(len(diags[0])):
        if len(diags) == 1:
            break
        wanted_bit = pref_bit if list(zip(*diags))[bit].count("1") >= len(diags) / 2 else {"0":"1", "1":"0"}[pref_bit]
        diags = [diag for diag in diags if diag[bit] == wanted_bit]
    return diags[0]


def star2(diags: List[str]) -> int:
    return int(lsr(diags, "1"), 2) * int(lsr(diags, "0"), 2)


if __name__ == "__main__":
    file_name = sys.argv[1] if len(sys.argv) == 2 else "input.txt"

    with open(file_name, "r") as f:
        diagnostics = f.read().splitlines()

    print(f"Star 1: {star1(diagnostics)}")
    print(f"Star 2: {star2(diagnostics)}")
