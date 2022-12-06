package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
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

func findMarkerPosition(input string, length int) int {
	for idx := length; idx < len(input); idx++ {
		set := map[rune]struct{}{}

		for _, chr := range input[idx-length : idx] {
			set[chr] = struct{}{}
		}

		if len(set) == length {
			return idx
		}
	}

	return 0
}

func solve(input *os.File) (int, int) {
	// Read whole file
	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	inputStr := strings.TrimSpace(string(inputBytes))

	return findMarkerPosition(inputStr, 4), findMarkerPosition(inputStr, 14)
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

	// Open file
	file, err := os.Open(*inputFile)
	panicOnErr(err)
	defer file.Close()

	// Solve
	s1, s2 := solve(file)
	fmt.Printf("Star 1: %v\n", s1)
	fmt.Printf("Star 2: %v\n", s2)
}
