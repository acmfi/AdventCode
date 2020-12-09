package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	file, _ := ioutil.ReadFile(os.Args[1])
	list := strings.Split((string(file)), "\n")
	var input []int
	for _, v := range list {
		item, _ := strconv.Atoi(v)
		input = append(input, item)
	}
	// Preamble subslice
	preamble := input[:25]
	// Part 1
	var init, part1 int
	for i := len(preamble); i < len(input); i++ {
		//fmt.Println("Number: ", input[i])
		result := twoSum(input[init:i], input[i])
		if result == 0 {
			part1 = input[i]
			break
		}
		init++
	}
	// Part 2
	contiguous := contiguousSum(input, part1)
	sort.Ints(contiguous)
	part2 := contiguous[0] + contiguous[len(contiguous)-1]
	fmt.Printf("Day 9\nPart 1: %d\nPart 2: %d\n", part1, part2)
}

func sumSlice(input []int) (sum int) {
	for _, value := range input {
		sum += value
	}
	return
}

func contiguousSum(input []int, target int) (result []int) {
	for i := 0; i < len(input); i++ {
		sumResult := sumSlice(result)
		//fmt.Println(input[i], result, sumResult)
		if sumResult == target {
			break
		} else if sumResult > target {
			result = result[1:]
			i--
		} else {
			result = append(result, input[i])
		}
	}
	return
}

func twoSum(numbers []int, target int) (result int) {
	if len(numbers) == 1 {
		return 0
	}
	for i := 1; i < len(numbers); i++ {
		subtarget := numbers[i] + numbers[0]
		if subtarget == target {
			//fmt.Printf("%d + %d = %d\n", numbers[i], numbers[0], subtarget)
			return numbers[i] + numbers[0]
		}
	}
	return twoSum(numbers[1:], target)
}
