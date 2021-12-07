package main

import (
	"flag"
	"fmt"
	"math"
	"os"
	"sort"
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

func solve(positions []int, constant bool) int {
	minFuel := math.MaxInt32

	sort.Ints(positions)
	for target := positions[0]; target <= positions[len(positions)-1]; target++ {
		fuel := 0

		for _, from := range positions {
			dist := target - from
			if dist < 0 {
				dist = -dist
			}

			if constant {
				fuel += dist
			} else {
				// Moving from to target requires 1 unit of fuel each step
				// starting in 1 unit in the first step. Step 1 requires 1 unit,
				// step 2 requires 2 units, step 3 requires 3 units, ...
				// For exampple 2 to 5 requires 3 steps:
				// 1 (2->3) + 2 (3->4) + 3 (3->5) = 1 + 2 + 3
				// Slow version: for f := 0; f <= dist; f++ { fuel += f }
				fuel += dist * (dist + 1) / 2
			}
		}

		if fuel < minFuel {
			minFuel = fuel
		}
	}

	return minFuel
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
	positions := []int{}

	for _, nstr := range strings.Split(strings.TrimSpace(string(input)), ",") {
		n, err := strconv.Atoi(nstr)
		panicOnErr(err)

		positions = append(positions, n)
	}

	// Solve
	fmt.Printf("Star 1: %d\n", solve(positions, true))
	fmt.Printf("Star 2: %d\n", solve(positions, false))
}
