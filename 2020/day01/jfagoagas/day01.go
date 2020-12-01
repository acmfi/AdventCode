package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	inputFile := readLines(os.Args[1])
	target, _ := strconv.Atoi(os.Args[2])
	fmt.Printf("Part 1 - Result: %d\n", twoSum(inputFile, target))
	fmt.Printf("Part 2 - Result: %d\n", threeSum(inputFile, target))
}

func threeSum(numbers []int, target int) (result int) {
	for i := 0; i < len(numbers); i++ {
		for j := i + 1; j < len(numbers); j++ {
			for k := j + 1; k < len(numbers); k++ {
				if numbers[i]+numbers[j]+numbers[k] == target {
					result = numbers[i] * numbers[j] * numbers[k]
					break
				}
			}
		}
	}
	return
}

func twoSum(numbers []int, target int) (result int) {
	for i := 0; i < len(numbers); i++ {
		for j := i + 1; j < len(numbers); j++ {
			if numbers[i]+numbers[j] == target {
				result = numbers[i] * numbers[j]
				break
			}
		}
	}
	return
}

func readLines(path string) []int {
	file, err := os.Open(path)
	if err != nil {
		fmt.Println("Can't open the input file for reading")
		os.Exit(1)
	}
	defer func() {
		err := file.Close()
		if err != nil {
			fmt.Println("Can't close file")
			os.Exit(1)
		}
	}()

	var lines []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line, _ := strconv.Atoi(scanner.Text())
		lines = append(lines, line)
	}
	return lines
}
