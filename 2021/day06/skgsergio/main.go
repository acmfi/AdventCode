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

func solve(ages []int, days int) uint64 {
	// Create a map of fishes with a given age
	fish := map[int]uint64{}
	for _, age := range ages {
		fish[age] += 1
	}

	// Simulate life cycles for the given days
	for day := 0; day < days; day++ {
		// Save fish with age 0 and set them to 0
		zeros := fish[0]
		fish[0] = 0

		// Decrement fish ages
		for age := 1; age <= 8; age++ {
			fish[age-1] += fish[age]
			fish[age] = 0
		}

		// Fish with age 0 become with age 6 and spawns fish with age 8
		fish[6] += zeros
		fish[8] += zeros
	}

	// Sum total number of fish
	var total uint64 = 0
	for _, count := range fish {
		total += count
	}

	return total
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
	ages := []int{}

	for _, nstr := range strings.Split(strings.TrimSpace(string(input)), ",") {
		n, err := strconv.Atoi(nstr)
		panicOnErr(err)

		ages = append(ages, n)
	}

	// Solve
	fmt.Printf("Star 1: %d\n", solve(ages, 80))
	fmt.Printf("Star 2: %d\n", solve(ages, 256))
}
