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

func (fold *Point) FoldDot(dot Point) Point {
	foldedDot := Point{dot.x, dot.y}

	if fold.x > 0 && dot.x > fold.x {
		foldedDot.x = fold.x*2 - dot.x
	}

	if fold.y > 0 && dot.y > fold.y {
		foldedDot.y = fold.y*2 - dot.y
	}

	return foldedDot
}

func drawDots(points map[Point]bool) string {
	// Search max dimension
	max := Point{0, 0}
	for p := range points {
		if p.x > max.x {
			max.x = p.x
		}
		if p.y > max.y {
			max.y = p.y
		}
	}

	// Print dots
	res := ""
	for y := 0; y <= max.y; y++ {
		row := ""

		for x := 0; x <= max.x; x++ {
			if points[Point{x, y}] {
				row += "█"
			} else {
				row += "░"
			}
		}

		res += row + "\n"
	}

	return res
}

func foldDots(dots map[Point]bool, fold Point) map[Point]bool {
	foldedDots := map[Point]bool{}

	// Apply the fold to all dots
	for dot := range dots {
		foldedDots[fold.FoldDot(dot)] = true
	}

	return foldedDots
}

func solve(dots map[Point]bool, folds []Point) (int, string) {
	visibleDots := []map[Point]bool{dots}

	// Run each fold with previous visible dots and store the resulting ones
	for _, fold := range folds {
		visibleDots = append(visibleDots, foldDots(visibleDots[len(visibleDots)-1], fold))
	}

	// Return:
	//   - Star 1: the number of visible points for the first fold
	//   - Star 2: a representation of the visible points for the last fold
	return len(visibleDots[1]), drawDots(visibleDots[len(visibleDots)-1])
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
	dots := map[Point]bool{}
	folds := []Point{}

	for _, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		if line == "" {
			continue
		}

		if line[0] == 'f' {
			fold := strings.Split(strings.Split(strings.TrimSpace(line), " ")[2], "=")
			c, err := strconv.Atoi(fold[1])
			panicOnErr(err)

			p := Point{0, 0}

			if fold[0] == "x" {
				p.x = c
			} else {
				p.y = c
			}

			folds = append(folds, p)
		} else {
			coords := strings.Split(strings.TrimSpace(line), ",")
			x, err := strconv.Atoi(coords[0])
			panicOnErr(err)
			y, err := strconv.Atoi(coords[1])
			panicOnErr(err)

			dots[Point{x, y}] = true
		}
	}

	// Solve
	star1, star2 := solve(dots, folds)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2:\n%s", star2)
}
