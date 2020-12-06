package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	input := readLines(os.Args[1])
	treeNumber := calcTreesNumberProbabilty(input, 3, 1)
	fmt.Printf("Day02 - Part 1: %d\n", treeNumber)

	var treeNumberProbability int = 0
	for _, value := range []struct {
		right int
		down  int
	}{
		{1, 1},
		{3, 1},
		{5, 1},
		{7, 1},
		{1, 2},
	} {
		probability := calcTreesNumberProbabilty(input, value.right, value.down)
		if treeNumberProbability == 0 {
			treeNumberProbability = probability
		} else {
			treeNumberProbability = treeNumberProbability * probability
		}
	}
	fmt.Printf("Day02 - Part 2: %d\n", treeNumberProbability)
}

func calcTreesNumberProbabilty(slope []string, right, down int) (treeNumberProbability int) {
	// [0,0]
	x := right
	y := down
	// x limit
	max := len(slope[y]) - 1
	// Check trees
	for y < len(slope) {
		object := slope[y][x]
		if string(object) == "#" {
			treeNumberProbability++
		}
		// Move X
		if x == max {
			x = right - 1
		} else if (x + right) > max {
			pos := max - x
			x = right - pos - 1
		} else {
			x = x + right
		}
		// Move Y
		y = y + down
	}
	return treeNumberProbability
}

func readLines(path string) []string {
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

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
	}
	return lines
}
