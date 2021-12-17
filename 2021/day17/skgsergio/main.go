package main

import (
	"flag"
	"fmt"
	"os"
	"regexp"
	"strconv"

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

func (p *Point) Ge(other Point) bool {
	return p.x >= other.x && p.y >= other.y
}

func (p *Point) Le(other Point) bool {
	return p.x <= other.x && p.y <= other.y
}

func solve(min, max Point) (int, int) {
	highestY := 0
	uniqueVels := make(map[Point]bool)

	// This is pure dice rolling
	loopMaxY := max.y * 10
	if loopMaxY < 0 {
		loopMaxY *= -1
	}

	for x := 0; x <= max.x; x++ {
		for y := min.y; y < loopMaxY; y++ {
			maxY := 0

			vel := Point{x, y}
			probe := Point{0, 0}

			for probe.Add(vel).x <= max.x && probe.Add(vel).y >= min.y {
				// Increase probe velocity
				probe = probe.Add(vel)

				// Decrease velocity due to drag
				vel.y -= 1

				if vel.x < 0 {
					vel.x += 1
				} else if vel.x > 0 {
					vel.x -= 1
				}

				// Store maximum Y if current probe y is greater
				if probe.y > maxY {
					maxY = probe.y
				}
			}

			// If the probe is within the limits store the highest Y of we've found
			// a greater Y value and store current initial velocity.
			if probe.Ge(min) && probe.Le(max) {
				if maxY > highestY {
					highestY = maxY
				}

				uniqueVels[Point{x, y}] = true
			}
		}
	}

	return highestY, len(uniqueVels)
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
	min := Point{0, 0}
	max := Point{0, 0}

	re := regexp.MustCompile(`.*x=(?P<MINX>-?\d+)..(?P<MAXX>-?\d+), y=(?P<MINY>-?\d+)..(?P<MAXY>-?\d+).*`)
	match := re.FindStringSubmatch(string(input))

	for i, name := range re.SubexpNames() {
		if name == "" {
			continue
		}

		c, err := strconv.Atoi(match[i])
		panicOnErr(err)

		switch name {
		case "MINX":
			min.x = c
		case "MAXX":
			max.x = c
		case "MINY":
			min.y = c
		case "MAXY":
			max.y = c
		}
	}

	// Solve
	star1, star2 := solve(min, max)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
