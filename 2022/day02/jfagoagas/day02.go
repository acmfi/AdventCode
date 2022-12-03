package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"sort"
	"strings"
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

func readLines(path string) []round {
	file, err := os.Open(path)
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()

	scanner := bufio.NewScanner(file)

	var lines []round
	for scanner.Scan() {
		if scanner.Text() == "" {
			continue
		}
		line := strings.Split(scanner.Text(), " ")
		lines = append(lines, round{elf: line[0], you: line[1]})
	}
	return lines
}

// ROCK 		A Y --> 1 point
// PAPER		B X --> 2 points
// SCISSORS		C Z --> 3 points

// WON --> 6 points
// DRAW --> 3 points
// LOSE --> 0 points

var shapeScore map[string]map[string]int = map[string]map[string]int{
	"A": {"X": 4, "Y": 8, "Z": 3},
	"B": {"X": 1, "Y": 5, "Z": 9},
	"C": {"X": 7, "Y": 2, "Z": 6},
}

type round struct {
	you string
	elf string
}

func part1(input []round) int {
	totalScore := 0
	for _, value := range input {
		// fmt.Printf("Round: %d\n", round+1)
		// fmt.Printf("Elf: %s\n", value.elf)
		// fmt.Printf("You: %s\n\n", value.you)
		totalScore += shapeScore[value.elf][value.you]
	}
	return totalScore
}

func part2(input []round) int {
	totalScore := 0
	for _, value := range input {
		you := value.you
		elf := value.elf
		// fmt.Printf("Round: %d\n", round+1)
		// fmt.Printf("Elf: %s\n", elf)
		// fmt.Printf("You: %s\n\n", you)

		points := getShapePoints(shapeScore[elf])
		switch {
		case you == "X":
			// LOSE, so lower value
			totalScore += points[0]
		case you == "Y":
			// DRAW, so intermediate value
			totalScore += points[1]
		case you == "Z":
			// WON, so greatest value
			totalScore += points[2]
		}
	}
	return totalScore
}

// Only three possibilites, win, draw, lose
func getShapePoints(shapePoints map[string]int) []int {
	var points []int
	for _, val := range shapePoints {
		points = append(points, val)
	}
	sort.Ints(points)
	return points
}

func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)

	fmt.Printf("Part 1 - Result: %d\n", part1(input))
	fmt.Printf("Part 2 - Result: %d\n", part2(input))
}
