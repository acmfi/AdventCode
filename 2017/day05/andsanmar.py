with open("input", "r") as file:
    for line in file:
        nums = list(map(lambda x: int(x), line.split()))
        pointer = 0
        lastPointer = pointer
        steps = 0
        while (pointer < len(nums) and pointer >= 0):
            pointer = pointer + nums[pointer]
            nums[lastPointer] = nums[lastPointer] + 1
            lastPointer = pointer
            steps = steps + 1
        print(steps)
