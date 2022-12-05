package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"regexp"
	"strings"

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
		// Replace empty spots with [X]
		line := strings.ReplaceAll(scanner.Text(), "    ", "[X]")
		lineNoSpaces := strings.ReplaceAll(line, " ", "")
		lines = append(lines, lineNoSpaces)
	}
	return lines
}

type stack []string

type movement struct {
	count       int
	origin      int
	destination int
}

func parseMovements(input []string) ([]movement, int, int) {
	startMovementsDeclaration := false
	var movements []movement
	maxElementsStack := 0
	var stacksNumber int
	var err error
	for index, value := range input {
		// Movements starts after the empty line
		if value == "" {
			maxElementsStack = index - 1
			startMovementsDeclaration = true
			// Count total stacks
			stacksIDs := strings.TrimSpace(input[index-1])
			stacksNumber, err = strconv.Atoi(string(stacksIDs[len(stacksIDs)-1]))
			exitOnError(err)
			continue
		}
		if startMovementsDeclaration {
			count, origin, destination := parseMovementPositions(value)
			movements = append(movements, movement{
				origin:      origin,
				destination: destination,
				count:       count,
			})
		}

	}
	return movements, maxElementsStack, stacksNumber
}

// parseMovementPositions returns the count, origin and destination of the movements
// input --> move 1 from 2 to 1
// output --> 1,2,1
func parseMovementPositions(input string) (int, int, int) {
	r := regexp.MustCompile(`[0-9]{1,}`)
	positions := r.FindAllString(input, -1)
	count, err := strconv.Atoi(positions[0])
	exitOnError(err)
	origin, err := strconv.Atoi(positions[1])
	exitOnError(err)
	destination, err := strconv.Atoi(positions[2])
	exitOnError(err)
	return count, origin, destination
}

// Return the stacks with the numeric ID and the stack using a slice
func parseStacks(input []string, maxElementsStack, stacksNumber int) map[int][]string {
	stacks := make(map[int][]string)
	// Init stacks
	for i := maxElementsStack - 1; i >= 0; i-- {
		// Retrieve stack values
		replacer := strings.NewReplacer("[", "", "]", "")
		cleanLine := replacer.Replace(input[i])
		for j := 0; j < stacksNumber; j++ {
			value := string(cleanLine[j])
			if value != "X" {
				stacks[j+1] = append(stacks[j+1], value)
			}
		}
	}
	return stacks
}

func getTopCrates(movements []movement, stacks map[int][]string, stacksNumber int, part1 bool) string {
	result := ""
	for _, mov := range movements {
		var items []string
		// fmt.Printf("Movement: %d\n", mov)
		// fmt.Printf("Origin Stack: %d %s\n", mov.origin, stacks[mov.origin])
		items = append(items, stacks[mov.origin][len(stacks[mov.origin])-mov.count:]...)
		// fmt.Printf("Items to move: %s\n", items)

		// Push to the destination
		// PART 1 - Stack
		if part1 {
			for i := len(items) - 1; i >= 0; i-- {
				stacks[mov.destination] = append(stacks[mov.destination], items[i])
			}
			// Part 2 - Preserve order
		} else {
			stacks[mov.destination] = append(stacks[mov.destination], items...)
		}

		// Pop from the origin
		stacks[mov.origin] = stacks[mov.origin][:len(stacks[mov.origin])-mov.count]

		// fmt.Printf("Origin Stack after pop: %d %s\n", mov.origin, stacks[mov.origin])
		// fmt.Printf("Destination Stack: %d %s\n\n", mov.destination, stacks[mov.destination])

	}

	// Recover the top elements
	for j := 0; j < stacksNumber; j++ {
		result += stacks[j+1][len(stacks[j+1])-1]
	}
	return result
}

func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)

	// Parse stacks and movements
	movements, maxElementsStack, stacksNumber := parseMovements(input)
	stacks := parseStacks(input, maxElementsStack, stacksNumber)

	fmt.Printf("Part 1 - Result: %s\n", getTopCrates(movements, stacks, stacksNumber, true))
	fmt.Printf("Part 2 - Result: %s\n", getTopCrates(movements, stacks, stacksNumber, false))
}
