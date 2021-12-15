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
	}
}

func findLowRiskPath(cave map[Point]int) int {
	// Set starting and ending points:
	//   start: top-left corner
	//   end: bottom-right corner
	start := Point{0, 0}
	end := Point{0, 0}
	for p := range cave {
		if p.x > end.x {
			end.x = p.x
		}
		if p.y > end.y {
			end.y = p.y
		}
	}

	// Calc the lower risk path to each point from the
	// starting point
	totalRisk := map[Point]int{start: 0}
	visits := []Point{start}

	for len(visits) > 0 {
		current := visits[0]
		visits = visits[1:]

		for _, next := range current.Adjacents() {
			if _, exists := cave[next]; !exists {
				continue
			}

			// If next has not been visited yet
			// or
			// If the sum of the total risk of current plus the risk of next point in the cave
			// is less than the total path risk of next
			// then
			// Store the total risk of current plus the risk of next point in the cave as the total
			// risk for the path to the next point
			_, visited := totalRisk[next]
			if !visited || totalRisk[current]+cave[next] < totalRisk[next] {
				totalRisk[next] = totalRisk[current] + cave[next]
				visits = append(visits, next)
			}
		}
	}

	// Return the minimum total risk for the destination point
	return totalRisk[end]
}

func expandCave(cave map[Point]int, scale int) map[Point]int {
	// Find max dimension
	max := Point{0, 0}
	for p := range cave {
		if p.x > max.x {
			max.x = p.x
		}
		if p.y > max.y {
			max.y = p.y
		}
	}
	max.x += 1
	max.y += 1

	// Start expanding the cave
	exapndedCave := map[Point]int{}

	for y := 0; y < max.y*scale; y++ {
		for x := 0; x < max.x*scale; x++ {
			risk := cave[Point{x % max.x, y % max.y}] + x/max.x + y/max.y

			for risk > 9 {
				risk -= 9
			}

			exapndedCave[Point{x, y}] = risk
		}
	}

	return exapndedCave
}

func star1(cave map[Point]int) int {
	return findLowRiskPath(cave)
}

func star2(cave map[Point]int) int {
	return findLowRiskPath(expandCave(cave, 5))
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
	cave := map[Point]int{}

	for y, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		for x, value := range strings.TrimSpace(line) {
			risk, err := strconv.Atoi(string(value))
			panicOnErr(err)

			cave[Point{x, y}] = risk
		}
	}

	// Solve
	fmt.Printf("Star 1: %d\n", star1(cave))
	fmt.Printf("Star 2: %d\n", star2(cave))
}
