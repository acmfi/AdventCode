package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	input := readLines(os.Args[1])
	fmt.Printf("Part 1: %d\n", part1(input))
}

func part1(input []string) int {
	maxLength := len(input[0])
	numberBinary := ""

	// Iterate by columns
	for i := 0; i < maxLength; i++ {
		oneCount := 0
		zeroCount := 0
		// For each input binary number
		for _, value := range input {
			if string(value[i]) == "0" {
				zeroCount++
			} else {
				oneCount++
			}
		}
		if zeroCount > oneCount {
			numberBinary += "0"
		} else {
			numberBinary += "1"
		}
	}
	negativeBinary := ""
	for i := range numberBinary {
		if numberBinary[i] == '0' {
			negativeBinary += "1"
		} else {
			negativeBinary += "0"
		}
	}
	number, _ := strconv.ParseInt(numberBinary, 2, 64)
	negative, _ := strconv.ParseInt(negativeBinary, 2, 64)
	return int(number * negative)
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
		//line, _ := strconv.ParseInt(scanner.Text(), 2, 64)
		lines = append(lines, scanner.Text())
	}
	return lines
}
