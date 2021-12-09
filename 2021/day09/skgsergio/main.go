package main

import (
	"encoding/hex"
	"flag"
	"fmt"
	"image"
	"image/color"
	"image/png"
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

func plot(points map[Point]int, size image.Rectangle, scale int) {
	colors := []color.RGBA{}
	for _, c := range []string{
		"5c53a5", "8c56a3", "b25c9c", "ce6693", "e3768a",
		"f18a82", "f8a07e", "fab880", "f8cf8a", "f3e79b",
	} {
		b, err := hex.DecodeString(c)
		panicOnErr(err)
		colors = append(colors, color.RGBA{b[0], b[1], b[2], 255})
	}

	img := image.NewRGBA(image.Rectangle{size.Min.Mul(scale), size.Max.Mul(scale)})

	for x := size.Min.X; x < size.Max.X; x++ {
		for y := size.Min.Y; y < size.Max.Y; y++ {
			color := color.RGBA{0, 0, 0, 0}
			if value, ok := points[Point{x, y}]; ok {
				color = colors[value]
			}

			for xs := 0; xs < scale; xs++ {
				for ys := 0; ys < scale; ys++ {
					img.Set((x*scale)+xs, (y*scale)+ys, color)
				}
			}
		}
	}

	f, err := os.Create("plot.png")
	panicOnErr(err)

	png.Encode(f, img)

	f.Close()
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

	cx := 0
	cy := 0
	for y, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		for x, value := range strings.TrimSpace(line) {
			height, err := strconv.Atoi(string(value))
			panicOnErr(err)

			points[Point{x, y}] = height

			cx = x + 1
		}

		cy = y + 1
	}

	plot(points, image.Rect(0, 0, cx, cy), 2)

	// Solve
	star1, star2 := solve(points)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
