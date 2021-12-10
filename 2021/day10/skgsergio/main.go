package main

import (
	"flag"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/pkg/profile"
)

var (
	inputFile = flag.String("f", "input.txt", "puzzle input file")
	profiling = flag.String("profile", "", "enable profiler, specify 'cpu' or 'mem'")
)

func panicOnErr(err error) {
	if err != nil {
		panic(err)
	}
}

var (
	charPairs = map[string]string{
		"(": ")",
		"[": "]",
		"{": "}",
		"<": ">",
	}

	charErrorScores = map[string]int{
		")": 3,
		"]": 57,
		"}": 1197,
		">": 25137,
	}

	charCompletionScores = map[string]int{
		")": 1,
		"]": 2,
		"}": 3,
		">": 4,
	}
)

func solve(lines [][]string) (int, int) {
	errorScore := 0
	completionScores := []int{}

	for _, line := range lines {
		open := []string{}
		invalid := false

		for _, chr := range line {
			// If the current char is a key in our char pairs map then
			// is an oppening character, otherwise is a closing character.
			if _, ok := charPairs[chr]; ok {
				// Add the open character to the list.
				open = append(open, chr)
			} else {
				// Check if the current character corresponds to the
				// closing character of the last open character.
				if chr != charPairs[open[len(open)-1]] {
					invalid = true
					errorScore += charErrorScores[chr]
					break
				}

				// Remove open character from the open list.
				open = open[:len(open)-1]
			}
		}

		// Star 2: Valid lines are incomplete, autocomplete them.
		if invalid {
			continue
		}

		completionScore := 0
		for i := len(open) - 1; i >= 0; i-- {
			// For each remaining open character, from last to first,
			// multiplythe current score by 5 and then add the value
			// of the corresponding closing character.
			completionScore *= 5
			completionScore += charCompletionScores[charPairs[open[i]]]
		}
		completionScores = append(completionScores, completionScore)
	}

	sort.Ints(completionScores)

	return errorScore, completionScores[len(completionScores)/2]
}

func main() {
	flag.Parse()

	// Profiler
	switch *profiling {
	case "cpu":
		defer profile.Start(profile.CPUProfile, profile.ProfilePath(".")).Stop()
	case "mem":
		defer profile.Start(profile.MemProfile, profile.ProfilePath(".")).Stop()
	}

	// Read file
	input, err := os.ReadFile(*inputFile)
	panicOnErr(err)

	// Parse input
	lines := [][]string{}
	for _, l := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		lines = append(lines, strings.Split(strings.TrimSpace(l), ""))
	}

	// Solve
	star1, star2 := solve(lines)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
