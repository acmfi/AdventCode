package main

import (
	"flag"
	"fmt"
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

func basinSize(points map[Point]int, start Point) int {
	// Expand basin by visiting the adjacent points of the
	// starting point and the adjacents of its adjacents and
	// so on while we find points that are not walls (points
	// with height 9).
	size := 1

	visit := []Point{start}
	seen := map[Point]bool{start: true}

	for len(visit) > 0 {
		visiting := visit[0]
		visit = visit[1:len(visit)]

		// Visit current point adjacents.
		for _, adj := range visiting.Adjacents() {
			// Skip the point if we already visited it.
			if seen[adj] {
				continue
			}
			seen[adj] = true

			// Check if the adjacent point exists (the point might be in an edge).
			height, ok := points[adj]
			if !ok {
				continue
			}

			// If the point is not a wall increase the basin size and
			// add it to the visit list for exanding the basin in that
			// direction.
			if height < 9 {
				size++
				visit = append(visit, adj)
			}
		}
	}

	return size
}

func solve(points map[Point]int) (int, int) {
	totalRisk := 0
	basinSizes := []int{}

	for current, height := range points {
		adjCount := 0
		lowerCount := 0

		// For each point check if it is lower than its adjacents.
		for _, adj := range current.Adjacents() {
			// Check if the adjacent point exists (the point might be in an edge).
			if adjHeight, ok := points[adj]; ok {
				adjCount++

				if height < adjHeight {
					lowerCount++
				}
			}
		}

		// If the current point is lower than its adjacents:
		// Star 1: sum to the total risk its height plus one
		// Star 2: calc the basin size it forms by visiting adjacent
		// points recursively until walls (points with height 9) are
		// surrounding the basin.
		if adjCount == lowerCount {
			totalRisk += height + 1
			basinSizes = append(basinSizes, basinSize(points, current))
		}
	}

	sort.Sort(sort.Reverse(sort.IntSlice(basinSizes)))

	return totalRisk, basinSizes[0] * basinSizes[1] * basinSizes[2]
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
	points := map[Point]int{}

	for y, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		for x, value := range strings.TrimSpace(line) {
			height, err := strconv.Atoi(string(value))
			panicOnErr(err)

			points[Point{x, y}] = height
		}
	}

	// Solve
	star1, star2 := solve(points)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
