package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strconv"
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

func readLines(path string) []string {
	file, err := os.Open(path)
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()

	scanner := bufio.NewScanner(file)

	var lines []string
	for scanner.Scan() {
		if scanner.Text() == "" {
			continue
		}
		lines = append(lines, scanner.Text())
	}
	return lines
}

type assignment struct {
	pair1 []int
	pair2 []int
}

func parseRange(input string) []int {
	pairNumbers := strings.Split(input, "-")

	start, err := strconv.Atoi(pairNumbers[0])
	exitOnError(err)

	end, err := strconv.Atoi(pairNumbers[1])
	exitOnError(err)

	pair := []int{}
	for i := start; i <= end; i++ {
		pair = append(pair, i)
	}
	return pair
}

func parseAssignments(input []string) []assignment {
	var assignments []assignment
	for _, line := range input {
		pairs := strings.Split(line, ",")
		assignments = append(assignments, assignment{pair1: parseRange(pairs[0]), pair2: parseRange(pairs[1])})
	}
	return assignments
}

func calcPairOverlap(assignments []assignment) (int, int) {
	fullyOverlapCount := 0
	overlapCount := 0
	for _, assignment := range assignments {
		pair1inPair2 := 0
		isPair1inPair2 := false
		pairOverlap := false
		for _, val1 := range assignment.pair1 {
			for _, val2 := range assignment.pair2 {
				if val1 < val2 {
					break
				}
				if val1 == val2 {
					pair1inPair2++
					isPair1inPair2 = true
				}
			}
			// PART 2
			// Check of one pair contains at least on value from the other pair
			if isPair1inPair2 && !pairOverlap {
				overlapCount++
				pairOverlap = true
			}
			// PART 1
			// Check if one pair contains the other
			if pair1inPair2 == len(assignment.pair1) || pair1inPair2 == len(assignment.pair2) {
				fullyOverlapCount++
				break
			}
		}
	}
	return fullyOverlapCount, overlapCount
}
func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)
	// Parse Assignments
	assignments := parseAssignments(input)
	// Calc assignments overlaps
	part1, part2 := calcPairOverlap(assignments)

	fmt.Printf("Part 1 - Result: %d\n", part1)
	fmt.Printf("Part 2 - Result: %d\n", part2)
}
