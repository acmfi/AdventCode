package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Segment struct {
	Point1 Point
	Point2 Point
}

type Point struct {
	x int
	y int
}

type inputPoints []Segment

func main() {
	inputMap := readLines(os.Args[1])
	fmt.Printf("Part 1: %d\n", calcLines(inputMap, false))
	fmt.Printf("Part 2: %d\n", calcLines(inputMap, true))
}

func calcLines(input inputPoints, diagonal bool) int {
	var Map map[Point]int = make(map[Point]int)
	for _, segment := range input {
		if segment.Point1.x == segment.Point2.x {
			// Same X's

			// Include From and To Points
			Map[Point{x: segment.Point1.x, y: segment.Point1.y}]++
			Map[Point{x: segment.Point2.x, y: segment.Point2.y}]++

			// Count distance
			countPoints := math.Abs(float64(segment.Point1.y-segment.Point2.y)) - 1

			// Check direction
			direction := 1
			if segment.Point1.y > segment.Point2.y {
				direction = -1
			}

			// Generate points in between
			for countPoints > 0 {
				var point Point
				point.x = segment.Point1.x
				point.y = segment.Point1.y + (direction * int(countPoints))
				Map[point]++
				countPoints--
			}
		} else if segment.Point1.y == segment.Point2.y {
			// Same Y's

			// Include From and To Points
			Map[Point{x: segment.Point1.x, y: segment.Point1.y}]++
			Map[Point{x: segment.Point2.x, y: segment.Point2.y}]++

			// Count distance
			countPoints := math.Abs(float64(segment.Point1.x-segment.Point2.x)) - 1

			// Check direction
			direction := 1
			if segment.Point1.x > segment.Point2.x {
				direction = -1
			}

			// Generate points in between
			for countPoints > 0 {
				var point Point
				point.x = segment.Point1.x + (direction * int(countPoints))
				point.y = segment.Point1.y

				// Include these points
				Map[point]++
				countPoints--
			}
		} else if diagonal {

			// Include From and To Points
			Map[Point{x: segment.Point1.x, y: segment.Point1.y}]++
			Map[Point{x: segment.Point2.x, y: segment.Point2.y}]++

			// Count distance
			countPoints := math.Abs(float64(segment.Point1.x-segment.Point2.x)) - 1

			// Check direction
			directionX, directionY := 1, 1
			if segment.Point1.x > segment.Point2.x {
				directionX = -1
			}
			if segment.Point1.y > segment.Point2.y {
				directionY = -1
			}

			// Generate points in between
			for countPoints > 0 {
				var point Point
				point.x = segment.Point1.x + (directionX * int(countPoints))
				point.y = segment.Point1.y + (directionY * int(countPoints))
				// Include these points
				Map[point]++
				countPoints--
			}
		}
	}
	counter := 0
	for _, sumLines := range Map {
		if sumLines >= 2 {
			counter++
		}
	}
	return counter
}

func readLines(path string) inputPoints {
	file, err := os.Open(path)
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

	var inputMap inputPoints
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.Split(scanner.Text(), " -> ")
		firstPoint := strings.Split(line[0], ",")
		secondPoint := strings.Split(line[1], ",")
		var segment Segment
		segment.Point1.x, _ = strconv.Atoi(firstPoint[0])
		segment.Point1.y, _ = strconv.Atoi(firstPoint[1])
		segment.Point2.x, _ = strconv.Atoi(secondPoint[0])
		segment.Point2.y, _ = strconv.Atoi(secondPoint[1])
		inputMap = append(inputMap, segment)
	}
	return inputMap
}
