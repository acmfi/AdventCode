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

var adjacent []Point = []Point{
	{0, 1},
	{1, 1},
	{1, 0},
	{1, -1},
	{0, -1},
	{-1, -1},
	{-1, 0},
	{-1, 1},
}

func (point *Point) Add(new Point) Point {
	return Point{point.x + new.x, point.y + new.y}
}

func main() {
	energyLevel := parseInput(os.Args[1])

	// Flash counter
	countFlash := 0
	star1, star2 := calcFlashes(energyLevel, countFlash)

	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}

func calcFlashes(energyLevel map[Point]int, countFlash int) (int, int) {
	star1 := 0
	star2 := 0

	// First step
	step := 1
	for {
		// At every step track who flash
		flashed := map[Point]struct{}{}
		// Iterate every point
		for point := range energyLevel {
			countFlash = flash(point, energyLevel, flashed, countFlash)
		}

		// Star 1, check how many flashes
		if step == 100 {
			star1 = countFlash
		}

		// Star 2, when all flashes simultaneously
		if len(flashed) == len(energyLevel) {
			star2 = step
			break
		}

		step++
	}

	return star1, star2
}

func flash(point Point, energyLevel map[Point]int, flashed map[Point]struct{}, countFlash int) int {
	// An octopus can only flash at most once per step
	if _, ok := flashed[point]; ok {
		return countFlash
	}

	// First, the energy level of each octopus increases by 1
	energyLevel[point]++

	// Then, any octopus with an energy level greater than 9 flashes.
	if energyLevel[point] > 9 {
		countFlash++
		flashed[point] = struct{}{}
		energyLevel[point] = 0

		/* This increases the energy level of all adjacent octopuses by 1,
		including octopuses that are diagonally adjacent. */
		for _, adjacentPoint := range adjacent {
			newPoint := point.Add(adjacentPoint)
			if _, ok := energyLevel[newPoint]; ok {
				/* If this causes an octopus to have an energy level greater than 9,
				it also flashes. This process continues as long as new octopuses keep
				having their energy level increased beyond 9 */
				countFlash = flash(newPoint, energyLevel, flashed, countFlash)
			}
		}
	}
	return countFlash
}

func parseInput(inputPath string) map[Point]int {
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

	energyLevel := map[Point]int{}

	scanner := bufio.NewScanner(file)
	x, y := 0, 0
	point := Point{}
	for scanner.Scan() {
		point.y = y
		line := strings.TrimSpace(scanner.Text())
		for _, value := range line {
			point.x = x
			energy, _ := strconv.Atoi(string(value))
			energyLevel[point] = energy
			x++
		}
		x = 0
		y++
	}
	return energyLevel
}
