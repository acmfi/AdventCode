package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	signalsPatterns := readLines(os.Args[1])
	fmt.Printf("Part 1: %d\n", calcEasyDigits(signalsPatterns))
}

type Pattern struct {
	inputSignals []string
	outputValues []string
}

func calcEasyDigits(input []Pattern) int {
	counter := 0
	for _, pattern := range input {
		for _, signals := range pattern.outputValues {
			switch len(signals) {
			case 2:
				counter++
			case 3:
				counter++
			case 4:
				counter++
			case 7:
				counter++
			}
		}
	}
	return counter
}

func readLines(path string) []Pattern {
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

	var signalsOutput []Pattern
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.Split(strings.TrimSpace(scanner.Text()), "|")
		leftPart := strings.Split(strings.TrimSpace(line[0]), " ")
		rightPart := strings.Split(strings.TrimSpace(line[1]), " ")
		pattern := Pattern{
			inputSignals: leftPart,
			outputValues: rightPart,
		}
		signalsOutput = append(signalsOutput, pattern)
	}
	return signalsOutput
}
