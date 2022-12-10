package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strings"

	"strconv"
)

var (
	inputFile = flag.String("f", "input.txt", "Puzzle input file")
)

func exitOnError(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func readLines(path string) []string {
	file, err := os.Open(path)
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()

	scanner := bufio.NewScanner(file)

	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type Movement struct {
	direction string
	steps     int
}

type Point struct {
	x int
	y int
}

func (p1 *Point) Dir() Point {
	direction := Point{0, 0}
	if p1.x > 0 {
		direction.x = 1
	} else if p1.x < 0 {
		direction.x = -1
	}

	if p1.y > 0 {
		direction.y = 1
	} else if p1.y < 0 {
		direction.y = -1
	}
	return direction
}

func (p1 *Point) Sub(p2 Point) Point {
	return Point{x: p1.x - p2.x, y: p1.y - p2.y}
}

func (p1 *Point) Add(p2 Point) Point {
	return Point{x: p1.x + p2.x, y: p1.y + p2.y}
}

type Rope struct {
	head    Point
	headMap map[Point]struct{}
	tail    Point
	tailMap map[Point]struct{}
}

func parseMovements(input []string) []Movement {
	movements := []Movement{}
	for _, movement := range input {
		values := strings.Split(movement, " ")
		direction := values[0]
		steps, err := strconv.Atoi(values[1])
		exitOnError(err)
		movements = append(movements, Movement{
			direction: direction,
			steps:     steps,
		})
	}
	return movements
}

var directions = map[string]Point{
	"U": {0, 1},
	"D": {0, -1},
	"L": {-1, 0},
	"R": {1, 0},
}

func moveRope(rope Rope, movements []Movement, actualPoint Point) {
	for _, movement := range movements {
		fmt.Println(movement)

		for i := 0; i < movement.steps; i++ {
			// previousPoint := Point{x: rope.head.x, y: rope.head.y}
			// Update Point
			actualPoint.x += directions[movement.direction].x
			actualPoint.y += directions[movement.direction].y
			// Update Rope head
			rope.head = Point{x: actualPoint.x, y: actualPoint.y}
			fmt.Printf("Head: %+v - Tail: %+v\n", rope.head, rope.tail)

			// Otherwise, if the head and tail aren't touching and aren't in the same row or column,
			// the tail always moves one step diagonally to keep up:
			if i != 0 {
				distancePoint := rope.head.Sub(rope.tail)
				fmt.Printf("Point distance %+v\n", distancePoint)
				// If the head is ever two steps directly up, down, left, or right from the tail,
				// the tail must also move one step in that direction so it remains close enough
				if distancePoint.x >= 2 || distancePoint.x <= -2 {
					// fmt.Println("NOT TOUCHING")
					// What direction?
					direction := distancePoint.Dir()
					fmt.Println(direction)
					// Update Tail
					rope.tail.x = rope.tail.x + direction.x
					rope.tail.y = rope.head.y
				} else if distancePoint.y >= 2 || distancePoint.y <= -2 {
					direction := distancePoint.Dir()
					fmt.Println(direction)
					// Update Tail
					rope.tail.x = rope.head.x
					rope.tail.y = rope.tail.y + direction.y
				}
			}

			// Update Maps with the head and the knot
			rope.headMap[rope.head] = struct{}{}
			rope.tailMap[rope.tail] = struct{}{}

			fmt.Printf("Tail: %+v\n\n", rope.tail)
			// printRopePoint(rope)
		}
		fmt.Printf("End movement status - H:%+v - T:%+v\n\n", rope.head, rope.tail)
	}
}

func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)
	// Parse Movements
	movements := parseMovements(input)

	// Assuming starts in the 0,0, so 1 visit
	actualPoint := Point{
		x: 0,
		y: 0,
	}
	// Create Rope
	rope := Rope{
		// Set the initial point
		head: actualPoint,
		headMap: map[Point]struct{}{
			actualPoint: {},
		},
		// Set the initial point
		tail: actualPoint,
		tailMap: map[Point]struct{}{
			actualPoint: {},
		},
	}

	moveRope(rope, movements, actualPoint)
	fmt.Printf("Part 1 - Result: %d\n", len(rope.tailMap))
}

func printRopePoint(rope Rope) {
	for y := 4; y >= 0; y-- {
		for x := 0; x <= 5; x++ {
			if rope.tail.x == x && rope.tail.y == y {
				fmt.Printf("X")
			} else {
				fmt.Printf(".")
			}

		}
		fmt.Println()
	}
}

func printRopeMap(headMap, tailMap map[Point]struct{}) {
	fmt.Println()
	for y := 4; y >= 0; y-- {
		for x := 0; x <= 5; x++ {
			if x == 0 && y == 0 {
				fmt.Printf("s")
				continue
			}

			_, okTail := tailMap[Point{x: x, y: y}]

			if okTail {
				fmt.Printf("#")
			} else {
				fmt.Printf(".")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}
