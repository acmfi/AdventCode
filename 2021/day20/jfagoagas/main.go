package main

import (
	"flag"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

var (
	input = flag.String("f", "input", "Puzzle input file")
	star1 int
	star2 int
)

type Point struct {
	x int
	y int
}

func (point Point) Add(new Point) Point {
	return Point{point.x + new.x, point.y + new.y}
}

func (point Point) Adjacents() []Point {
	adjacents := []Point{}
	for _, adj := range adjacent {
		checkPoint := point.Add(adj)
		adjacents = append(adjacents, checkPoint)
	}
	return adjacents
}

var pixelToBinary map[string]string = map[string]string{
	".": "0",
	"#": "1",
}

var adjacent []Point = []Point{
	{-1, -1},
	{-1, 0},
	{-1, 1},
	{0, -1},
	{0, 0},
	{0, 1},
	{1, -1},
	{1, 0},
	{1, 1},
}

func countLitPixels(image map[Point]string) int {
	lightPixels := 0
	for _, pixel := range image {
		if pixel == "#" {
			lightPixels++
		}
	}
	return lightPixels
}

func filterImage(imageEnhancementAlgorithm []string, image map[Point]string, times int) {
	for i := 0; i < times; i++ {
		// Minimum and maximum image points
		maxPoint := Point{x: 0, y: 0}
		minPoint := Point{x: math.MaxInt, y: math.MaxInt}
		for point := range image {
			if point.x > maxPoint.x {
				maxPoint.x = point.x
			}
			if point.y > maxPoint.y {
				maxPoint.y = point.y
			}
			if point.x < minPoint.x {
				minPoint.x = point.x
			}
			if point.y < minPoint.y {
				minPoint.y = point.y
			}
		}

		// Temporary image
		tmpImage := map[Point]string{}

		// Iterate over every point of our image and increse 1 each
		for x := minPoint.x - 1; x <= maxPoint.x+1; x++ {
			for y := minPoint.y - 1; y <= maxPoint.y+1; y++ {
				point := Point{x: x, y: y}

				// Image filter
				filter := ""

				// For each point check their adjacent pixels
				for _, adjPoint := range point.Adjacents() {
					// If this pixel exists, store it
					if pixel, ok := image[adjPoint]; ok {
						filter += pixel
					} else {
						// Every odd iteration any new point contains #
						if i%2 == 1 {
							filter += "#"
						} else {
							filter += "."
						}
					}
				}

				// Binary filter
				binaryFilter := ""
				for _, value := range filter {
					if binary, ok := pixelToBinary[string(value)]; ok {
						binaryFilter += binary
					}
				}

				// Filter value
				filterValue, err := strconv.ParseInt(binaryFilter, 2, 10)
				if err != nil {
					fmt.Println(err)
					os.Exit(1)
				}

				// Filter pixel
				// Substitue each point in the new image with their value at the image enhancement algorithm
				tmpImage[point] = imageEnhancementAlgorithm[int(filterValue)]

			}
		}
		image = tmpImage
		// Star 1
		if i == 1 {
			star1 = countLitPixels(tmpImage)
		}
		/* The image enhancement algorithm describes how to enhance an image
		by simultaneously converting all pixels in the input image into an output image. */

	}
	star2 = countLitPixels(image)
}

func main() {
	flag.Parse()
	imageEnhancementAlgorithm, image := parseInput(*input)
	filterImage(imageEnhancementAlgorithm, image, 50)

	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)

}

func parseInput(input string) ([]string, map[Point]string) {
	file, err := os.ReadFile(input)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	lines := strings.Split(strings.TrimSpace(string(file)), "\n")

	imageEnhancementAlgorithm := []string{}
	for _, value := range lines[0] {
		imageEnhancementAlgorithm = append(imageEnhancementAlgorithm, string(value))
	}

	image := map[Point]string{}
	for x, value := range lines[2:] {
		for y, pixel := range value {
			image[Point{
				x: x,
				y: y,
			}] = string(pixel)

		}
	}
	return imageEnhancementAlgorithm, image
}
