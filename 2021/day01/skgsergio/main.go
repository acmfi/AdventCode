package main

import (
	"flag"
	"fmt"
	"os"
	"strconv"
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

func solve(depths []int) (int, int) {
	count1 := 0
	count2 := 0

	for i := 1; i < len(depths); i++ {
		// Star 1: current > previous
		if depths[i] > depths[i-1] {
			count1++
		}

		// Star 2: current + next 2 > previous + current + next
		if i+2 < len(depths) && depths[i]+depths[i+1]+depths[i+2] > depths[i-1]+depths[i]+depths[i+1] {
			count2++
		}
	}

	return count1, count2
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
	depths := []int{}
	for _, s := range strings.Fields(string(input)) {
		i, err := strconv.Atoi(s)
		panicOnErr(err)
		depths = append(depths, i)
	}

	// Solve
	s1, s2 := solve(depths)
	fmt.Printf("Star 1: %d\n", s1)
	fmt.Printf("Star 2: %d\n", s2)
}
