#Escrito en Cython

"""
import pyximport; pyximport.install(pyimport= True, build_dir = "/home/user/Desktop")
print("Finished compiling\n\t---===---")

import J_advent_of_code

print(Day1.day1_p1("/home..."))
print(Day1.day1_p2("/home..."))
"""

cdef int c_p1(file_location):
	cdef str s
	cdef list nums

	with open(file_location, "r") as f:
		s = f.read()
	nums = [int(number) for number in s.split("\n")]
	return sum(nums)

cdef int c_p2(file_location):
	has_been_found = False
	cdef int current_freq = 0, result = 0, counter = 0
	cdef str s
	cdef list nums
	cdef set frequencies = set([current_freq])

	with open(file_location, "r") as f:
		s = f.read()

	nums = [int(number) for number in s.split("\n")]
	cdef int len_nums = len(nums)
	while not has_been_found:
		current_freq += nums[counter % len_nums]
		if current_freq in frequencies:
			has_been_found = True
			result = current_freq
		frequencies.add(current_freq)
		counter += 1
	return result

def day1_p1(file_location):
	return c_p1(file_location)

def day1_p2(file_location):
	return c_p2(file_location)