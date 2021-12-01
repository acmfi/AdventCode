from sys import stdin

nums = list(map(int, stdin))
star1 = sum([int(t[1] > t[0]) for t in zip(nums[:-1], nums[1:])])
star2 = sum([int(sum(nums[i+1:i+4]) > sum(nums[i:i+3])) for i in range(len(nums) - 2)])
print(star1, star2)
