import sys

from typing import List, Tuple, Callable

def bit_count(diags: List[str], bit: int) -> Tuple[int, int]:
    ones = sum(diag[bit] == "1" for diag in diags)
    return ones, len(diags)-ones


def star1(diags: List[str]) -> int:
    gamma = ""
    epsilon = ""

    for bit in range(len(diags[0])):
        ones, zeros = bit_count(diags, bit)

        gamma += "1" if ones >= zeros else "0"
        epsilon += "0" if ones >= zeros else "1"

    return int(gamma, 2) * int(epsilon, 2)


def filter_diags(diags: List[str], bit: int, bit_criteria: Callable[[int, int], str]) -> List[str]:
    # If there is just one diagnostic we do not need filtering
    if len(diags) == 1:
        return diags

    # Find the wanted bit in base to the ones and zeros according to a given criteria
    ones, zeros = bit_count(diags, bit)
    wanted_bit = bit_criteria(ones, zeros)

    # Keep only the diagnostics with the matching bit value
    return [d for d in diags if d[bit] == wanted_bit]


def star2(diags: List[str]) -> int:
    o2 = diags
    co2 = diags

    for bit in range(len(diags[0])):
        # O2 Generator Rating
        o2 = filter_diags(o2, bit, lambda ones, zeros: "0" if ones < zeros else "1")
        # CO2 Scrubber Rating
        co2 = filter_diags(co2, bit, lambda ones, zeros: "1" if ones < zeros else "0")

    return int(o2[0], 2) * int(co2[0], 2)


if __name__ == "__main__":
    file_name = sys.argv[1] if len(sys.argv) == 2 else "input.txt"

    with open(file_name, "r") as f:
        diagnostics = f.read().splitlines()

    print(f"Star 1: {star1(diagnostics)}")
    print(f"Star 2: {star2(diagnostics)}")
