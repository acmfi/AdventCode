package main

import (
	"flag"
	"fmt"
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

type Status rune

const (
	EMPTY Status = '.'
	EAST  Status = '>'
	SOUTH Status = 'v'
)

type Point struct {
	x int
	y int
}

func solve(floor map[Point]Status) (int, string) {
	// Find dimensions
	length := Point{0, 0}
	for p := range floor {
		if p.x >= length.x {
			length.x = p.x + 1
		}
		if p.y >= length.y {
			length.y = p.y + 1
		}
	}

	steps := 0
	for {
		steps++

		// Move herd east
		eastHerd := []Point{}
		for p, c := range floor {
			if c == EAST && floor[Point{(p.x + 1) % length.x, p.y}] == EMPTY {
				eastHerd = append(eastHerd, p)
			}
		}

		for _, p := range eastHerd {
			floor[p] = EMPTY
			floor[Point{(p.x + 1) % length.x, p.y}] = EAST
		}

		// Move herd south
		southHerd := []Point{}
		for p, c := range floor {
			if c == SOUTH && floor[Point{p.x, (p.y + 1) % length.y}] == EMPTY {
				southHerd = append(southHerd, p)
			}
		}

		for _, p := range southHerd {
			floor[p] = EMPTY
			floor[Point{p.x, (p.y + 1) % length.y}] = SOUTH
		}

		// If no movements were performed then we've finished
		if len(eastHerd)+len(southHerd) == 0 {
			break
		}
	}

	return steps, "CLICK!"
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
	floor := map[Point]Status{}

	for y, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		for x, chr := range strings.TrimSpace(line) {
			floor[Point{x, y}] = Status(chr)
		}
	}

	// Solve
	star1, star2 := solve(floor)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %s\n", star2)
}
