package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
)

var (
	inputPath = flag.String("f", "input", "Puzzle input path")
)

type state string

const (
	east  state = ">"
	south state = "v"
	empty state = "."
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

type point struct {
	x int
	y int
}

func calcEdge(cucumbers map[point]state) point {
	p := point{x: 0, y: 0}
	for point := range cucumbers {
		if point.x > p.x {
			p.x = point.x
		}
		if point.y > p.y {
			p.y = point.y
		}
	}
	return p
}

// Print cucumbers map
func cucumbersMap(cucumbers map[point]state) {
	edge := calcEdge(cucumbers)
	for x := 0; x <= edge.x; x++ {
		for y := 0; y <= edge.y; y++ {
			fmt.Print(cucumbers[point{
				x: x,
				y: y,
			}])
		}
		fmt.Println()
	}
}

func checkEdge(p, edge point, direction string) point {
	// Much easier with % (modulo) operation
	nextPoint := point{}
	// Check if the next step is an edge
	if direction == "y" {
		if p.y == edge.y {
			nextPoint.x = p.x
			nextPoint.y = 0
		} else {
			nextPoint.x = p.x
			nextPoint.y = p.y + 1
		}
	} else {
		if p.x == edge.x {
			nextPoint.x = 0
			nextPoint.y = p.y
		} else {
			nextPoint.x = p.x + 1
			nextPoint.y = p.y
		}
	}
	return nextPoint
}

func calcLand(cucumbers map[point]state) int {
	edge := calcEdge(cucumbers)
	step := 0
	for {
		step++

		// First, EAST moving cucumbers
		eastHerd := []point{}
		// Check how many moving cucumbers can move forward in this step
		for p, state := range cucumbers {
			if state == east {
				// Check if the next step is an edge
				nextPoint := checkEdge(p, edge, "y")

				// Check if the next step is an empty space
				next := cucumbers[nextPoint]

				// Move cucumber if the next point is an empty space
				if next == empty {
					eastHerd = append(eastHerd, p)
				}
			}
		}
		// Move EAST moving cucumbers
		for _, p := range eastHerd {
			nextPoint := checkEdge(p, edge, "y")
			cucumbers[p] = empty
			cucumbers[nextPoint] = east
		}

		// Second, SOUTH moving cucumbers
		southHerd := []point{}
		// Check how many moving cucumbers can move forward in this step
		for p, state := range cucumbers {
			if state == south {
				// Check if the next step is an edge
				nextPoint := checkEdge(p, edge, "x")
				// Check if the next step is an empty space
				next := cucumbers[nextPoint]
				// Move cucumber if the next point is an empty space
				if next == empty {
					southHerd = append(southHerd, p)
				}
			}
		}
		for _, p := range southHerd {
			nextPoint := checkEdge(p, edge, "x")
			cucumbers[p] = empty
			cucumbers[nextPoint] = south
		}
		// If no sea cucumbers move --> We have place to land!
		if len(eastHerd) == 0 && len(southHerd) == 0 {
			break
		}
	}
	return step
}

func main() {
	flag.Parse()
	cucumbers := parseInput(*inputPath)
	fmt.Printf("Star 1: %d\n", calcLand(cucumbers))
}

func parseInput(inputPath string) map[point]state {
	file, err := os.Open(inputPath)
	check(err)
	defer func() {
		err := file.Close()
		check(err)
	}()

	cucumbers := map[point]state{}
	scanner := bufio.NewScanner(file)
	x := 0
	for scanner.Scan() {
		p := point{}
		y := 0
		for _, value := range scanner.Text() {
			p.x = x
			p.y = y
			cucumbers[p] = state(value)
			y++
		}
		x++
	}
	return cucumbers
}
