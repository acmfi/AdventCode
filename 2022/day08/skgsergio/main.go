package main

import (
	"bufio"
	"encoding/hex"
	"flag"
	"fmt"
	"image"
	"image/color"
	"image/png"
	"os"
	"strconv"

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

func plot(points map[Point]int, scale int, name string) {
	// Gradient based on DarkMint CARTOColors
	colorsHex := []string{
		"D2FBD4", "BDE6C6", "A7D1B9", "92BCAB", "7DA79E",
		"679390", "527E83", "3D6975", "275468", "123F5A",
		"FF0000",
	}

	// Convert HEX colors to RGBA
	colors := make([]color.RGBA, len(colorsHex))
	for idx := range colorsHex {
		b, err := hex.DecodeString(colorsHex[idx])
		panicOnErr(err)
		colors[idx] = color.RGBA{b[0], b[1], b[2], 255}
	}

	// Find max max
	max := Point{0, 0}
	for p := range points {
		if p.x > max.x {
			max.x = p.x
		}
		if p.y > max.y {
			max.y = p.y
		}
	}

	// Create canvas initializing all points to White
	img := image.NewRGBA(image.Rect(0, 0, (max.x+1)*scale, (max.y+1)*scale))
	for x := img.Rect.Min.X; x <= img.Rect.Max.X; x++ {
		for y := img.Rect.Min.Y; y <= img.Rect.Max.Y; y++ {
			img.Set(x, y, color.White)
		}
	}

	// Print points by value
	for p, value := range points {
		// Convert HEX color to RGB values
		for xs := 0; xs < scale; xs++ {
			for ys := 0; ys < scale; ys++ {
				img.Set((p.x*scale)+xs, (p.y*scale)+ys, colors[value])
			}
		}
	}

	// Save file
	f, err := os.Create(fmt.Sprintf("%s.png", name))
	panicOnErr(err)
	defer f.Close()

	png.Encode(f, img)
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	// Read file line by line
	patch := map[Point]int{}

	scanner := bufio.NewScanner(input)
	y := 0
	for scanner.Scan() {
		s := scanner.Text()

		for x, chr := range s {
			n, err := strconv.Atoi(string(chr))
			panicOnErr(err)

			patch[Point{x, y}] = n
		}

		y++
	}
	panicOnErr(scanner.Err())

	visibleTrees := map[Point]int{} // Just for ploting purposes

	dirs := []Point{{0, -1}, {0, 1}, {-1, 0}, {1, 0}}
	for currTree, currHeight := range patch {
		visible := false
		score := 1

		for _, dir := range dirs {
			for nextTree, dist := currTree.Add(dir), 0; ; nextTree, dist = nextTree.Add(dir), dist+1 {
				nextHeight, exists := patch[nextTree]

				if !exists {
					visible = true
					score *= dist
					break
				}

				if nextHeight >= currHeight {
					score *= dist + 1
					break
				}
			}
		}

		if visible {
			visibleTrees[currTree] = currHeight
			s1 += 1
		}

		if score > s2 {
			s2 = score
		}
	}

	plot(patch, 4, "patch")
	plot(visibleTrees, 4, "visible")
	combined := map[Point]int{}
	for tree, height := range patch {
		combined[tree] = height
		if _, visible := visibleTrees[tree]; visible {
			combined[tree] = 10
		}
	}
	plot(combined, 4, "combined")

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
