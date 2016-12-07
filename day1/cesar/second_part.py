def get_input():
    with open("./input") as INPUT:
        return INPUT.readline().strip()


def get_new_direction(current, turn):
    if current == "up":
        if turn == "R":
            return "right"
        elif turn == "L":
            return "left"
    elif current == "down":
        if turn == "R":
            return "left"
        elif turn == "L":
            return "right"
    elif current == "left":
        if turn == "R":
            return "up"
        elif turn == "L":
            return "down"
    elif current == "right":
        if turn == "R":
            return "down"
        elif turn == "L":
            return "up"


def list_to_tuple_list(input_list):
    # Change: [ "R1", "L3", "L6", ... ] to [("R", 1), ("L", 3), ("L", 6), ...]
    if len(input_list) <= 0:
        return []
    head_list = input_list[0]
    head_tuple = (head_list[0], int(head_list[1:]))  # ("R", 1)
    return [head_tuple] + list_to_tuple_list(input_list[1:])


def get_list_until_collision(step_list):
    # We'll get a collision when there are 3 consecutive Rs or Ls, and
    # the distance of the third is greater or equal to the first one
    pass


up = 0
down = 0
left = 0
right = 0

# Can look: up, down, left, right
looking = "up"

raw_parsed_input = get_input().split(', ')

parsed_input = ""

# Main loop
for char in parsed_input:
    # If it's a turn
    if char.startswith("R") or char.startswith("L"):
        looking = get_new_direction(looking, char[0])

    # If it's a number
    digit = char[1:]
    if digit.isdigit():
        if looking == "up":
            up += int(digit)
        elif looking == "down":
            down += int(digit)
        elif looking == "left":
            left += int(digit)
        elif looking == "right":
            right += int(digit)

# Get coordinates
distance = abs(up - down) + abs(left - right)

print("The distance to the Easter Bunny HQ is: {}".format(distance))
