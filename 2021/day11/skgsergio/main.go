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

type Point struct {
	x int
	y int
}

func (p *Point) Add(other Point) Point {
	return Point{p.x + other.x, p.y + other.y}
}

func (p *Point) Adjacents() []Point {
	return []Point{
		p.Add(Point{0, 1}),
		p.Add(Point{0, -1}),
		p.Add(Point{1, 0}),
		p.Add(Point{-1, 0}),
		p.Add(Point{1, 1}),
		p.Add(Point{1, -1}),
		p.Add(Point{-1, 1}),
		p.Add(Point{-1, -1}),
	}
}

func flash(octopuses map[Point]int, octopus Point, flashed map[Point]bool, flashes int) int {
	// Do nothing if we already flashed this octopus.
	if flashed[octopus] {
		return flashes
	}

	// Increase the octopus energy, if it the energy level is less or equal to 9
	// don't flash it.
	octopuses[octopus]++
	if octopuses[octopus] <= 9 {
		return flashes
	}

	// Flash the octopus as the energy level exceeds 9, this consumes all the energy.
	flashes++
	flashed[octopus] = true
	octopuses[octopus] = 0

	// Expand the flash to the adjacent octopuses.
	for _, c := range octopus.Adjacents() {
		if _, ok := octopuses[c]; !ok {
			continue
		}

		flashes = flash(octopuses, c, flashed, flashes)
	}

	return flashes
}

func solve(octopuses map[Point]int) (int, int) {
	flashes := []int{0}
	steps := 0

	for {
		// Flash all octopuses in this step.
		flashed := map[Point]bool{}
		stepFlashes := flashes[steps]

		for octopus := range octopuses {
			stepFlashes = flash(octopuses, octopus, flashed, stepFlashes)
		}

		flashes = append(flashes, stepFlashes)

		steps++

		// Check if we are already in sync.
		inSync := true
		for _, energy := range octopuses {
			if energy > 0 {
				inSync = false
				break
			}
		}

		if inSync {
			break
		}
	}

	return flashes[100], steps
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
	octopuses := map[Point]int{}

	for y, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		for x, value := range strings.TrimSpace(line) {
			energy, err := strconv.Atoi(string(value))
			panicOnErr(err)

			octopuses[Point{x, y}] = energy
		}
	}

	// Solve
	star1, star2 := solve(octopuses)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
