package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
	"unicode"

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

func solve(input *os.File) (string, string) {
	s1 := ""
	s2 := ""

	// Read whole file
	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	// Split input parts
	inputParts := strings.Split(string(inputBytes), "\n\n")
	initialState := strings.Split(inputParts[0], "\n")
	movements := strings.Split(strings.TrimSpace(inputParts[1]), "\n")

	// Parse starting stacks crating two cranes one for each part
	cranes := [2]map[string]string{{}, {}}

	crateLevels := initialState[:len(initialState)-1]
	stackNumbers := initialState[len(initialState)-1]

	for _, level := range crateLevels {
		for idx, crate := range level {
			if !unicode.IsLetter(crate) {
				// Skip empty columns and markers
				continue
			}

			cranes[0][string(stackNumbers[idx])] += string(crate)
			cranes[1][string(stackNumbers[idx])] += string(crate)
		}
	}

	// Perform movements
	for _, movement := range movements {
		if movement == "" {
			continue
		}

		m := strings.Split(movement, " ")

		number, err := strconv.Atoi(m[1])
		panicOnErr(err)
		from := m[3]
		to := m[5]

		// Move one by one (part 1)
		for i := 0; i < number; i++ {
			cranes[0][to] = cranes[0][from][:1] + cranes[0][to]
			cranes[0][from] = cranes[0][from][1:]
		}

		// Move multiple at once (part 2)
		cranes[1][to] = cranes[1][from][:number] + cranes[1][to]
		cranes[1][from] = cranes[1][from][number:]
	}

	// Grab last crate of each stack
	for _, stack := range strings.Fields(stackNumbers) {
		s1 += cranes[0][stack][:1]
		s2 += cranes[1][stack][:1]
	}

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
	fmt.Printf("Star 1: %v\n", s1)
	fmt.Printf("Star 2: %v\n", s2)
}
