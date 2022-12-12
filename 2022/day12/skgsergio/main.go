package main

import (
	"flag"
	"fmt"
	"io/ioutil"
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

type Point struct {
	x, y int
}

func (p *Point) Add(q Point) Point {
	return Point{p.x + q.x, p.y + q.y}
}

type PointDist struct {
	point    Point
	distance int
}

func findShortestDistance(area map[Point]int, starts []Point, end Point) int {
	seen := map[Point]struct{}{}
	queue := []PointDist{}

	for _, start := range starts {
		seen[start] = struct{}{}
		queue = append(queue, PointDist{start, 0})
	}

	for {
		current := queue[0].point
		distance := queue[0].distance
		queue = queue[1:]

		if current == end {
			return distance
		}

		for _, dir := range []Point{{0, -1}, {0, 1}, {-1, 0}, {1, 0}} {
			next := current.Add(dir)

			if _, exists := area[next]; !exists {
				continue
			}
			if _, alreadySeen := seen[next]; alreadySeen {
				continue
			}

			if area[next] <= area[current]+1 {
				seen[next] = struct{}{}
				queue = append(queue, PointDist{next, distance + 1})
			}
		}
	}
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	area := map[Point]int{}
	start := Point{}
	end := Point{}

	for y, l := range strings.Split(strings.TrimSpace(string(inputBytes)), "\n") {
		for x, c := range l {
			if c == 'S' {
				start = Point{x, y}
				c = 'a'
			} else if c == 'E' {
				end = Point{x, y}
				c = 'z'
			}

			area[Point{x, y}] = int(c - 'a')
		}
	}

	s1 = findShortestDistance(area, []Point{start}, end)

	starts := []Point{}
	for point, height := range area {
		if height == 0 {
			starts = append(starts, point)
		}
	}
	s2 = findShortestDistance(area, starts, end)

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
