package main

import (
	"bufio"
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
	x, y int
}

func (p *Point) Add(q Point) Point {
	return Point{p.x + q.x, p.y + q.y}
}

func (p *Point) Sub(q Point) Point {
	return Point{p.x - q.x, p.y - q.y}
}

func (p *Point) Dir() Point {
	d := Point{0, 0}

	if p.x > 0 {
		d.x = 1
	} else if p.x < 0 {
		d.x = -1
	}

	if p.y > 0 {
		d.y = 1
	} else if p.y < 0 {
		d.y = -1
	}

	return d
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	movements := map[string]Point{
		"U": {0, -1},
		"D": {0, 1},
		"L": {-1, 0},
		"R": {1, 0},
	}

	knots := 10
	rope := make([]Point, knots)
	visited := [2]map[Point]struct{}{{}, {}}

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		s := strings.Split(scanner.Text(), " ")

		movement := s[0]
		steps, err := strconv.Atoi(s[1])
		panicOnErr(err)

		// Move head
		for i := 0; i < steps; i++ {
			rope[0] = rope[0].Add(movements[movement])

			// Move all knots
			for j := 1; j < len(rope); j++ {
				delta := rope[j-1].Sub(rope[j])
				if delta.x > 1 || delta.x < -1 || delta.y > 1 || delta.y < -1 {
					rope[j] = rope[j].Add(delta.Dir())
				}
			}

			// Mark current position as visited
			visited[0][rope[1]] = struct{}{}           // Second knot (part 1)
			visited[1][rope[len(rope)-1]] = struct{}{} // Last knot (part 2)
		}
	}
	panicOnErr(scanner.Err())

	s1 = len(visited[0])
	s2 = len(visited[1])

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
