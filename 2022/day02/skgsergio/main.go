package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"

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

func solve(input *os.File) (int, int) {
	//     Fmt: Oponent You
	// Oponent: A - Rock, B - Paper, C - Scissors
	//  You-S1: X - Rock, Y - Paper, C - Scissors (what you play)
	//  You-S2: X - Lose, Y - Draw, Z - Win (required outcome)
	// Scoring: (Your Move) + (Outcome)
	//         Values: Rock = 1, Paper = 2, Scissors = 3
	//                 Lost = 0, Draw = 3, Won = 3
	points := map[string][2]int{
		"A X": {1 + 3, 3 + 0},
		"A Y": {2 + 6, 1 + 3},
		"A Z": {3 + 0, 2 + 6},
		"B X": {1 + 0, 1 + 0},
		"B Y": {2 + 3, 2 + 3},
		"B Z": {3 + 6, 3 + 6},
		"C X": {1 + 6, 2 + 0},
		"C Y": {2 + 0, 3 + 3},
		"C Z": {3 + 3, 1 + 6},
	}

	s1 := 0
	s2 := 0

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		s := scanner.Text()

		s1 += points[s][0]
		s2 += points[s][1]
	}

	panicOnErr(scanner.Err())

	return s1, s2
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
	file, err := os.Open(*inputFile)
	panicOnErr(err)
	defer file.Close()

	// Solve
	s1, s2 := solve(file)
	fmt.Printf("Star 1: %d\n", s1)
	fmt.Printf("Star 2: %d\n", s2)
}
