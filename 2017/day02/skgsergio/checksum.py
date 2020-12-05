import sys


def checksum_spreadsheet(spreadsheet):
    checksum = 0

    for row in spreadsheet:
        checksum += (max(row) - min(row))

    return checksum


def checksum_spreadsheet_2(spreadsheet):
    checksum = 0

    for row in spreadsheet:
        row_len = len(row)

        for idx in range(row_len):
            rest = row[:idx] + row[idx+1:]

            for n in rest:
                if row[idx] % n == 0:
                    checksum += int(row[idx] / n)
                    break
                elif n % row[idx] == 0:
                    checksum += int(n % row[idx])
                    break

    return checksum


if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("Usage: {} <input_spreadsheet> [1/2]".format(sys.argv[0]),
              file=sys.stderr)

    spreadsheet = []
    with open(sys.argv[1], 'r') as f:
        for l in f:
            spreadsheet.append(list(map(int, l.split())))

    if sys.argv[2] == "1":
        print(checksum_spreadsheet(spreadsheet))

    elif sys.argv[2] == "2":
        print(checksum_spreadsheet_2(spreadsheet))
