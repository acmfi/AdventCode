package main

import (
	"fmt"
	"os"
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
