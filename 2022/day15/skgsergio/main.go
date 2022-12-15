package main

import (
	"bufio"
	"flag"
	"fmt"
	"math"
	"os"

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

func Abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

type Point struct {
	x, y int
}

func (p *Point) Add(q Point) Point {
	return Point{p.x + q.x, p.y + q.y}
}

func (p *Point) Dist(q Point) int {
	return Abs(p.x-q.x) + Abs(p.y-q.y)
}

type Report struct {
	sensor Point
	beacon Point
}

func (r *Report) Dist() int {
	return r.sensor.Dist(r.beacon)
}

func findImpossibles(reports []Report, row int) int {
	minX := math.MaxInt32
	maxX := math.MinInt32

	for _, report := range reports {
		d := report.Dist()

		if report.sensor.x-d < minX {
			minX = report.sensor.x - d
		}

		if report.sensor.x+d > maxX {
			maxX = report.sensor.x + d
		}
	}

	count := 0

	for x := minX; x < maxX+1; x++ {
		pos := Point{x, row}

		for _, report := range reports {
			if report.sensor.Dist(pos) <= report.Dist() && pos != report.beacon {
				count += 1
				break
			}
		}
	}

	return count
}

func findBeacon(reports []Report, min int, max int) int {
	visible := true
	beacon := Point{}

	for y := min; visible && y <= max; y++ {
		for x := min; visible && x <= max; x++ {
			visible = false
			beacon = Point{x, y}

			for _, report := range reports {
				reportDist := report.Dist()
				dist := report.sensor.Dist(beacon)

				if dist <= reportDist {
					visible = true
					x += reportDist - dist
					break
				}
			}
		}
	}

	return beacon.x*4_000_000 + beacon.y
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	reports := []Report{}

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		report := Report{}

		fmt.Sscanf(
			scanner.Text(),
			"Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d",
			&report.sensor.x, &report.sensor.y,
			&report.beacon.x, &report.beacon.y,
		)

		reports = append(reports, report)
	}
	panicOnErr(scanner.Err())

	s1 = findImpossibles(reports, 2_000_000)
	s2 = findBeacon(reports, 0, 4_000_000)

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
