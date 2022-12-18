package main

import (
	"bufio"
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
	x, y, z int
}

func (p *Point) Add(q Point) Point {
	return Point{p.x + q.x, p.y + q.y, p.z + q.z}
}

func possibleNeighbours(point Point, min int, max int) map[Point]struct{} {
	neighbours := map[Point]struct{}{}

	if point.x > min {
		neighbours[point.Add(Point{-1, 0, 0})] = struct{}{}
	}
	if point.x < max {
		neighbours[point.Add(Point{1, 0, 0})] = struct{}{}
	}

	if point.y > min {
		neighbours[point.Add(Point{0, -1, 0})] = struct{}{}
	}
	if point.y < max {
		neighbours[point.Add(Point{0, 1, 0})] = struct{}{}
	}

	if point.z > min {
		neighbours[point.Add(Point{0, 0, -1})] = struct{}{}
	}
	if point.z < max {
		neighbours[point.Add(Point{0, 0, 1})] = struct{}{}
	}

	return neighbours
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	droplet := map[Point]struct{}{}
	coords := []int{}

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		p := strings.Split(strings.TrimSpace(scanner.Text()), ",")

		x, err := strconv.Atoi(p[0])
		panicOnErr(err)
		y, err := strconv.Atoi(p[1])
		panicOnErr(err)
		z, err := strconv.Atoi(p[2])
		panicOnErr(err)

		coords = append(coords, []int{x, y, z}...)

		droplet[Point{x, y, z}] = struct{}{}
	}
	panicOnErr(scanner.Err())

	sort.Sort(sort.IntSlice(coords))
	min := coords[0] - 1
	max := coords[len(coords)-1] + 1

	// Part 1
	s1 = 6 * len(droplet)

	for p := range droplet {
		intersections := 0

		for neighbour := range possibleNeighbours(p, min, max) {
			if _, ok := droplet[neighbour]; ok {
				intersections += 1
			}
		}

		s1 -= intersections
	}

	// Part 2
	limits := []Point{{min, min, min}}
	steam := map[Point]struct{}{limits[0]: {}}

	for len(limits) > 0 {
		point := limits[len(limits)-1]
		limits = limits[:len(limits)-1]

		for neighbour := range possibleNeighbours(point, min, max) {
			if _, ok := steam[neighbour]; ok {
				continue
			}

			if _, ok := droplet[neighbour]; ok {
				s2 += 1
			} else {
				steam[neighbour] = struct{}{}
				limits = append(limits, neighbour)
			}
		}
	}

	return s1, s2
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
