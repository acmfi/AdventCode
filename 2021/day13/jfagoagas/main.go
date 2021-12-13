package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x int
	y int
}

func main() {
	dotsPosition, folds := parseInput(os.Args[1])

	var visiblePoints int
	for i, foldPoint := range folds {
		// Star 1
		if i == 0 {
			visiblePoints = foldPoints(dotsPosition, foldPoint)
			continue
		}
		_ = foldPoints(dotsPosition, foldPoint)
	}
	fmt.Printf("Star 1: %d\n", visiblePoints)
	fmt.Printf("Star 2:\n")

	drawInstructions(dotsPosition)
}

func calcMaxPoints(points map[Point]struct{}) Point {
	maxPoint := Point{}
	for point := range points {
		if point.x > maxPoint.x {
			maxPoint.x = point.x
		}
		if point.y > maxPoint.y {
			maxPoint.y = point.y
		}
	}

	return maxPoint
}

func foldPoints(dotsPosition map[Point]struct{}, foldPoint Point) int {
	maxPoint := calcMaxPoints(dotsPosition)

	for position := range dotsPosition {
		newPoint := Point{}

		if foldPoint.x == 0 {
			// horizontal line, fold the bottom half up

			// if the point is below fold line
			if position.y > foldPoint.y {
				newPoint.x = position.x
				newPoint.y = maxPoint.y - position.y
				// Delete this point
				delete(dotsPosition, Point{
					x: position.x,
					y: position.y,
				})
				// Append the new folded point
				dotsPosition[newPoint] = struct{}{}
			}
		} else {
			// vertical line, fold left

			// if the point is away fold line
			if position.x > foldPoint.x {
				newPoint.x = maxPoint.x - position.x
				newPoint.y = position.y

				// Delete this point
				delete(dotsPosition, Point{
					x: position.x,
					y: position.y,
				})
				// Append the new folded point
				dotsPosition[newPoint] = struct{}{}
			}
		}
	}
	return len(dotsPosition)
}

func drawInstructions(points map[Point]struct{}) {
	maxPoint := calcMaxPoints(points)
	for y := 0; y <= maxPoint.y; y++ {
		line := ""
		for x := 0; x <= maxPoint.x; x++ {
			point := Point{
				x: x,
				y: y,
			}
			if _, ok := points[point]; ok {
				line += "█"
			} else {
				line += "░"
			}
		}
		fmt.Println(line)
	}
}

func parseInput(inputPath string) (map[Point]struct{}, []Point) {
	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Println("Can't open the input file for reading")
		os.Exit(1)
	}
	defer func() {
		err := file.Close()
		if err != nil {
			fmt.Println("Can't close file")
			os.Exit(1)
		}
	}()

	input := map[Point]struct{}{}
	folds := []Point{}
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		if scanner.Text() == "" {
			continue
		}
		if scanner.Text()[0] == 'f' {
			line := strings.Split(strings.TrimSpace(scanner.Text()), " ")
			foldPosition := strings.Split(strings.TrimSpace(line[2]), "=")
			point := Point{}
			if foldPosition[0] == "x" {
				x, _ := strconv.Atoi(foldPosition[1])
				point.x = x
				point.y = 0
			} else {
				y, _ := strconv.Atoi(foldPosition[1])
				point.x = 0
				point.y = y
			}
			folds = append(folds, point)
		} else {
			line := strings.Split(strings.TrimSpace(scanner.Text()), ",")
			x, _ := strconv.Atoi(line[0])
			y, _ := strconv.Atoi(line[1])
			point := Point{
				x: x,
				y: y,
			}
			input[point] = struct{}{}
		}

	}
	return input, folds
}
