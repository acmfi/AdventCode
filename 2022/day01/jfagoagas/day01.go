package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"sort"
	"strconv"
)

var (
	inputFile = flag.String("f", "input.txt", "Puzzle input file")
)

func exitOnError(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func readLines(path string) []int {
	file, err := os.Open(path)
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()

	scanner := bufio.NewScanner(file)

	var lines []int
	for scanner.Scan() {
		if scanner.Text() == "" {
			lines = append(lines, 0)
			continue
		}
		line, _ := strconv.Atoi(scanner.Text())
		lines = append(lines, line)

	}
	return lines
}

func countCalories(cals []int) []int {
	var totalCalories []int
	currentCalories := 0
	for index, cal := range cals {
		currentCalories += cal
		// New Elf or last
		if cal == 0 || index == len(cals)-1 {
			totalCalories = append(totalCalories, currentCalories)
			currentCalories = 0
		}

	}
	sort.Ints(totalCalories)
	return totalCalories
}

func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)

	totalCalories := countCalories(input)
	fmt.Printf("Part 1 - Result: %d\n", totalCalories[len(totalCalories)-1])
	top3Calories := totalCalories[len(totalCalories)-1] + totalCalories[len(totalCalories)-2] + totalCalories[len(totalCalories)-3]
	fmt.Printf("Part 2 - Result: %d\n", top3Calories)
}
