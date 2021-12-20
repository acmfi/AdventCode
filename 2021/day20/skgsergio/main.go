package main

import (
	"flag"
	"fmt"
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
	x int
	y int
}

func (p *Point) Add(other Point) Point {
	return Point{p.x + other.x, p.y + other.y}
}

func (p Point) Window() []Point {
	return []Point{
		p.Add(Point{-1, -1}), p.Add(Point{0, -1}), p.Add(Point{1, -1}),
		p.Add(Point{-1, 0}), p.Add(Point{0, 0}), p.Add(Point{1, 0}),
		p.Add(Point{-1, 1}), p.Add(Point{0, 1}), p.Add(Point{1, 1}),
	}
}

func maxInt(x, y int) int {
	if x > y {
		return x
	}
	return y
}

func minInt(x, y int) int {
	if x < y {
		return x
	}
	return y
}

func countLitPixels(image map[Point]bool) int {
	count := 0
	for _, pixval := range image {
		if pixval {
			count++
		}
	}
	return count
}

func solve(algorithm []bool, image map[Point]bool) (int, int) {
	twoPass := 0

	for pass := 0; pass < 50; pass++ {
		passResult := map[Point]bool{}

		// Find min and max dimensions of the image
		min := Point{math.MaxInt, math.MaxInt}
		max := Point{0, 0}
		for pix := range image {
			min.x = minInt(min.x, pix.x)
			min.y = minInt(min.y, pix.y)
			max.x = maxInt(max.x, pix.x)
			max.y = maxInt(max.y, pix.y)
		}

		// Iterate all pixels, adding margin by increasing size
		for y := min.y - 1; y <= max.y+1; y++ {
			for x := min.x - 1; x <= max.x+1; x++ {
				algIdx := 0

				// Calc the algorithm index by extracting bits from pixel value of the
				// current piel window: for each pixel in the window (in order from
				// top-left to bottom-right) shift 1 bit and add 1 if the pixel is lit.
				for _, pix := range (Point{x, y}).Window() {
					pixval, ok := image[pix]

					algIdx <<= 1
					if (ok && pixval) || (!ok && pass%2 == 1) {
						algIdx |= 1
					}
				}

				passResult[Point{x, y}] = algorithm[algIdx]
			}
		}

		image = passResult

		// Star 1: 2 iterations
		if pass == 1 {
			twoPass = countLitPixels(image)
		}
	}

	// Star 2: 50 iterations
	return twoPass, countLitPixels(image)
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
	algorithm := []bool{}
	image := map[Point]bool{}

	lines := strings.Split(strings.TrimSpace(string(input)), "\n")
	for _, chr := range strings.TrimSpace(lines[0]) {
		algorithm = append(algorithm, chr == '#')
	}

	for y, line := range lines[2:] {
		line = strings.TrimSpace(line)
		for x, chr := range strings.TrimSpace(line) {
			image[Point{x, y}] = chr == '#'
		}
	}

	// Solve
	star1, star2 := solve(algorithm, image)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
