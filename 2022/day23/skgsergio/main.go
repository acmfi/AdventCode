package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"math"
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

func min(a int, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	elves := map[Point]struct{}{}

	for x, row := range strings.Split(string(inputBytes), "\n") {
		for y, chr := range row {
			if chr == '#' {
				elves[Point{x, y}] = struct{}{}
			}
		}
	}

	round := 0
	for {
		round++
		possibleMovements := map[Point]Point{}

		for pos := range elves {
			count := [4]int{}

			for _, dir := range []Point{
				{-1, -1}, {-1, 0}, {-1, 1},
				{0, -1}, {0, 1},
				{1, -1}, {1, 0}, {1, 1},
			} {
				if _, ok := elves[pos.Add(dir)]; ok {
					if dir.x == -1 {
						count[0]++
					}
					if dir.x == 1 {
						count[1]++
					}
					if dir.y == -1 {
						count[2]++
					}
					if dir.y == 1 {
						count[3]++
					}
				}
			}

			if count[0]+count[1]+count[2]+count[3] == 0 {
				continue
			}

			for j := range count {
				idx := (round - 1 + j) % 4
				if count[idx] == 0 {
					switch idx {
					case 0:
						possibleMovements[pos] = pos.Add(Point{-1, 0})
					case 1:
						possibleMovements[pos] = pos.Add(Point{1, 0})
					case 2:
						possibleMovements[pos] = pos.Add(Point{0, -1})
					case 3:
						possibleMovements[pos] = pos.Add(Point{0, 1})
					}
					break
				}
			}

		}

		destinationCount := map[Point]int{}
		for _, to := range possibleMovements {
			destinationCount[to]++
		}

		movements := 0
		for from, to := range possibleMovements {
			if destinationCount[to] == 1 {
				delete(elves, from)
				elves[to] = struct{}{}
				movements++
			}
		}

		// Part 2: No movements
		if movements == 0 {
			s2 = round
			break
		}

		// Part 1: 10 rounds
		if round == 10 {
			minx := math.MaxInt
			miny := math.MaxInt
			maxx := math.MinInt
			maxy := math.MinInt

			for pos := range elves {
				minx = min(minx, pos.x)
				miny = min(miny, pos.y)
				maxx = max(maxx, pos.x)
				maxy = max(maxy, pos.y)
			}

			for x := minx; x <= maxx; x++ {
				for y := miny; y <= maxy; y++ {
					if _, ok := elves[Point{x, y}]; !ok {
						s1++
					}
				}
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
