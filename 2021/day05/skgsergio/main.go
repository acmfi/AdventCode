package main

import (
	"flag"
	"fmt"
	"image"
	"image/color"
	"image/png"
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

type Line struct {
	from Point
	to   Point
}

func (l *Line) Direction() Point {
	dir := Point{0, 0}

	if l.from.x < l.to.x {
		dir.x = 1
	} else if l.from.x > l.to.x {
		dir.x = -1
	}

	if l.from.y < l.to.y {
		dir.y = 1
	} else if l.from.y > l.to.y {
		dir.y = -1
	}

	return dir
}

func (l *Line) Points() []Point {
	points := []Point{l.from}

	dir := l.Direction()
	next := l.from
	for next != l.to {
		next = next.Add(dir)
		points = append(points, next)
	}

	return points
}

func solve(lines []Line, diagonals bool) int {
	seenPoints := map[Point]int{}

	for _, line := range lines {
		direction := line.Direction()

		// Ignore diagonal lines
		if !diagonals && direction.x != 0 && direction.y != 0 {
			continue
		}

		// Generate all points between the from and to points of the line
		for _, point := range line.Points() {
			seenPoints[point]++
		}
	}

	// Count the overlapping points (that have been seen more than once)
	overlaped := 0
	for _, c := range seenPoints {
		if c > 1 {
			overlaped++
		}
	}

	// Plot!
	plot(seenPoints, image.Rect(0, 0, 1000, 1000), 1, diagonals)

	return overlaped
}

func plot(points map[Point]int, size image.Rectangle, scale int, diagonals bool) {
	img := image.NewRGBA(image.Rectangle{size.Min.Mul(scale), size.Max.Mul(scale)})

	for x := img.Rect.Min.X; x <= img.Rect.Max.X; x++ {
		for y := img.Rect.Min.Y; y <= img.Rect.Max.Y; y++ {
			img.Set(x, y, color.White)
		}
	}

	for pt, count := range points {
		c := color.RGBA{230, 230, 230, 255}
		if count > 1 {
			c = color.RGBA{255, 0, 0, 255}
		}

		for xs := 0; xs < scale; xs++ {
			for ys := 0; ys < scale; ys++ {
				img.Set((pt.x*scale)+xs, (pt.y*scale)+ys, c)
			}
		}
	}

	fname := "plot_without_diagonals.png"
	if diagonals {
		fname = "plot_with_diagonals.png"
	}
	f, err := os.Create(fname)
	panicOnErr(err)

	png.Encode(f, img)

	f.Close()
}

func parsePoint(point string) Point {
	coords := strings.Split(point, ",")

	x, err := strconv.Atoi(coords[0])
	panicOnErr(err)
	y, err := strconv.Atoi(coords[1])
	panicOnErr(err)

	return Point{x, y}
}

func parseLines(input string) []Line {
	lines := []Line{}

	for _, l := range strings.Split(strings.TrimSpace(input), "\n") {
		points := strings.Split(l, " -> ")

		lines = append(lines, Line{parsePoint(points[0]), parsePoint(points[1])})
	}

	return lines
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
	lines := parseLines(string(input))

	// Solve
	fmt.Printf("Star 1: %d\n", solve(lines, false))
	fmt.Printf("Star 2: %d\n", solve(lines, true))
}
