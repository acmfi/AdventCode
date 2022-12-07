package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
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
		lines = append(lines, scanner.Text())
	}
	return lines
}

func checkMarker(input string, length int) int {
	// Marker final position
	markerPosition := 0
	// We have to check markers in groups of length
	for i := 0; i < len(input)-length; i++ {
		markers := make(map[string]struct{})
		// Check the substring in groups of length
		marker := input[i : i+length]
		// Check if the substring has duplicate values
		for _, char := range marker {
			markers[string(char)] = struct{}{}
		}
		// If has not duplicates
		if len(markers) == length {
			markerPosition = i + length
			break

		}
	}
	return markerPosition
}

func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)[0]

	fmt.Printf("Part 1 - Result: %d\n", checkMarker(input, 4))
	fmt.Printf("Part 2 - Result: %d\n", checkMarker(input, 14))
}
