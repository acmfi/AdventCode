package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	input := readLines(os.Args[1])
	fmt.Printf("Part 1 - Result: %d\n", depthIncreases(input))
	fmt.Printf("Part 2 - Result: %d\n", depthIncreases2(input))
}

func depthIncreases(input []int) int {
	depthIncreases := 0
	depth := input[0]
	for _, value := range input {
		if value > depth {
			depthIncreases++
		}
		depth = value
	}
	return depthIncreases
}

func depthIncreases2(input []int) int {
	depthIncreases := 0
	depth := input[0]
	for i := 0; i < len(input)-3; i++ {
		value := input[i] + input[i+1] + input[i+2]
		if value > depth {
			depthIncreases++
		}
		depth = value
	}
	return depthIncreases
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
