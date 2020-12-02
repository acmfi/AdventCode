package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	input := readLines(os.Args[1])
	validOldPasswords := testOldPasswordPolicy(input)
	fmt.Printf("Day02 - Part 1: %d\n", validOldPasswords)
	validPasswords := testPasswordPolicy(input)
	fmt.Printf("Day02 - Part 2: %d\n", validPasswords)
}

func parseInput(input string) (min, max int, letter, password string) {
	values := strings.Split(input, " ")

	// save the min and max limits
	limits := strings.Split(values[0], "-")
	min, _ = strconv.Atoi(limits[0])
	max, _ = strconv.Atoi(limits[1])

	// letter
	letter = string(values[1][0])

	// password
	password = values[2]

	return
}

func testPasswordPolicy(input []string) (validPasswords int) {
	for i := range input {
		min, max, letter, password := parseInput(input[i])
		if (string(password[min-1]) == letter || string(password[max-1]) == letter) && (password[min-1] != password[max-1]) {
			validPasswords++
		}
	}
	return validPasswords
}

func testOldPasswordPolicy(input []string) (validPasswords int) {
	for i := range input {
		min, max, letter, password := parseInput(input[i])
		counter := 0
		for i := 0; i < len(password); i++ {
			if string(password[i]) == letter {
				counter++
			}
		}
		if counter <= max && counter >= min {
			validPasswords++
		}
	}
	return validPasswords
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
