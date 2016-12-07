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

up = 0
down = 0
left = 0
right = 0

# Can look: up, down, left, right
looking = "up"

# Main loop
for char in get_input():
    # Skip unwanted characters
    if char == "," or char == " ":
        continue

    # If it's a turn
    if char == "R" or char == "L":
        looking = get_new_direction(looking, char)

    # If it's a number
    if char.isdigit():
        if looking == "up":
            up += int(char)
        elif looking == "down":
            down += int(char)
        elif looking == "left":
            left += int(char)
        elif looking == "right":
            right += int(char)

# Get coordinates
distance = abs(up - down) + abs(left - right)

print("The distance to the Easter Bunny HQ is: {}".format(distance))
