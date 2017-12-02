import sys


def checksum_spreadsheet(spreadsheet):
    checksum = 0

    for row in spreadsheet:
        checksum += (max(row) - min(row))

    return checksum


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: {} <input_spreadsheet>".format(sys.argv[0]),
              file=sys.stderr)

    spreadsheet = []
    with open(sys.argv[1], 'r') as f:
        for l in f:
            spreadsheet.append(list(map(int, l.split())))

    print(checksum_spreadsheet(spreadsheet))
