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

func (p *Point) Dir(q Point) Point {
	d := Point{0, 0}

	if p.x > q.x {
		d.x = -1
	} else if p.x < q.x {
		d.x = 1
	}

	if p.y > q.y {
		d.y = -1
	} else if p.y < q.y {
		d.y = 1
	}

	return d
}

type Path struct {
	points []Point
}

type Material rune

const (
	Rock Material = '#'
	Sand          = 'o'
)

func simulation(caveSolids map[Point]Material, sandSource Point, floorExists bool) int {
	max := Point{0, 0}
	for p := range caveSolids {
		if p.x > max.x {
			max.x = p.x
		}

		if p.y > max.y {
			max.y = p.y
		}
	}

	abyssY := max.y + 1
	floorY := max.y + 2

	fallingSand := sandSource
	sandUnits := 0

	for {
		// Part 1: Sand falling to the abyss
		if !floorExists && fallingSand.y >= abyssY {
			break
		}

		// Part 2: Sand reached the floor
		if floorExists && caveSolids[sandSource] == Sand {
			break
		}

		// Check if the sand is still falling
		stillFalling := false

		for _, dir := range []Point{{0, 1}, {-1, 1}, {1, 1}} {
			next := fallingSand.Add(dir)

			// Part 2: Check if floor is reached
			if floorExists && next.y == floorY {
				break
			}

			// Check if next possible position is not a solid
			if _, isSolid := caveSolids[next]; !isSolid {
				stillFalling = true
				fallingSand = next
				break
			}
		}

		if stillFalling {
			continue
		}

		// New sand!
		caveSolids[fallingSand] = Sand
		fallingSand = sandSource
		sandUnits++
	}

	return sandUnits
}

func genCaveRocks(rockPaths []Path) map[Point]Material {
	caveRocks := map[Point]Material{}

	for _, path := range rockPaths {
		for i := 1; i < len(path.points); i++ {
			segmentStart := path.points[i-1]
			segmentEnd := path.points[i]
			dir := segmentStart.Dir(segmentEnd)

			for current := segmentStart; current != segmentEnd; current = current.Add(dir) {
				caveRocks[current] = Rock
			}

			caveRocks[segmentEnd] = Rock
		}
	}

	return caveRocks
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	rockPaths := []Path{}

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		s := scanner.Text()

		path := Path{}

		for _, p := range strings.Split(strings.TrimSpace(s), " -> ") {
			c := strings.Split(p, ",")

			x, err := strconv.Atoi(c[0])
			panicOnErr(err)
			y, err := strconv.Atoi(c[1])
			panicOnErr(err)

			path.points = append(path.points, Point{x, y})
		}

		rockPaths = append(rockPaths, path)
	}
	panicOnErr(scanner.Err())

	s1 = simulation(genCaveRocks(rockPaths), Point{500, 0}, false)
	s2 = simulation(genCaveRocks(rockPaths), Point{500, 0}, true)

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
