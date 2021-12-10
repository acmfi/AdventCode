package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

func main() {
	subsytemNavigation := readLines(os.Args[1])
	star1, star2 := validateSyntax(subsytemNavigation)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}

var (
	autocompleteScoreMap = map[rune]int{
		'(': 1,
		'[': 2,
		'{': 3,
		'<': 4,
	}
	syntaxErrorScoreMap = map[rune]int{
		')': 3,
		']': 57,
		'}': 1197,
		'>': 25137,
	}
)

func validateSyntax(input []string) (int, int) {
	// Star 1
	syntaxErrorScore := 0
	// Star 2
	var points []int
	for _, line := range input {
		var stack []rune
	newLine:
		for _, chunk := range line {

			if len(stack) == 0 && (chunk == ')' || chunk == ']' || chunk == '}' || chunk == '>') {
				syntaxErrorScore += syntaxErrorScoreMap[chunk]
				break
			} else if chunk == '(' || chunk == '[' || chunk == '{' || chunk == '<' {
				// PUSH
				stack = append(stack, chunk)
			} else {
				switch chunk {
				case ')':
					if stack[len(stack)-1] == '(' {
						// POP
						stack = stack[:len(stack)-1]
					} else {
						syntaxErrorScore += syntaxErrorScoreMap[chunk]
						stack = nil
						break newLine
					}
				case ']':
					if stack[len(stack)-1] == '[' {
						//POP
						stack = stack[:len(stack)-1]
					} else {
						syntaxErrorScore += syntaxErrorScoreMap[chunk]
						stack = nil
						break newLine
					}
				case '}':
					if stack[len(stack)-1] == '{' {
						//POP
						stack = stack[:len(stack)-1]
					} else {
						syntaxErrorScore += syntaxErrorScoreMap[chunk]
						stack = nil
						break newLine
					}
				case '>':
					if stack[len(stack)-1] == '<' {
						//POP
						stack = stack[:len(stack)-1]
					} else {
						syntaxErrorScore += syntaxErrorScoreMap[chunk]
						stack = nil
						break newLine
					}
				}
			}

		}
		// Star 2 -- Incomplete lines
		if len(stack) != 0 {
			autocompleteScore := 0
			for i := len(stack) - 1; i >= 0; i-- {
				autocompleteScore *= 5
				chunk := stack[i]
				autocompleteScore += autocompleteScoreMap[chunk]
			}
			points = append(points, autocompleteScore)
		}
	}
	sort.Ints(points)
	return syntaxErrorScore, points[len(points)/2]
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

	var input []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		input = append(input, scanner.Text())
	}
	return input
}
