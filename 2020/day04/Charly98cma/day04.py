import re

with open("input.txt", "r") as f:
    # Field 'cid' is optional, so its not on the list of required fields
    reqFields = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
    passport = dict()
    numValid = 0
    invalid = False
    for l in f:
        l = l.strip()
        if not l: # Empty lines (check passport)
            for field in reqFields:
                if field not in passport:
                    invalid = True
                    break
            if not invalid:
                numValid += 1
            # Reset the passport data
            passport = dict()
            # Reset the invalid flag
            invalid = False
        else: # Not empty lines (keep reading fields)
            l = re.split(r'[:| ]', l)
            it = iter(l)
            for key in it:
                passport[key] = next(it)

print("1st STAR SOLUTION",numValid)
