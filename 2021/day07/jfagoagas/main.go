package main

import (
	"fmt"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	crabPositions := readLines(os.Args[1])
	fmt.Printf("Part 1: %d\n", calcCrabFuel(crabPositions, true))
	fmt.Printf("Part 2: %d\n", calcCrabFuel(crabPositions, false))

}
func calcCrabFuel(crabPositions []int, constant bool) int {
	var fuelCost []int
	for _, position := range crabPositions {
		alignedPosition := position
		fuelCounter := 0
		for _, value := range crabPositions {
			distance := int(math.Abs(float64(alignedPosition - value)))
			if constant {
				fuelCounter += distance
			} else {
				for distance > 0 {
					fuelCounter += distance
					distance--
				}
			}
		}
		fuelCost = append(fuelCost, fuelCounter)
	}
	sort.Ints(fuelCost)
	return fuelCost[0]
}

func readLines(path string) []int {
	file, err := os.ReadFile(path)
	if err != nil {
		fmt.Println("Can't open the input file for reading")
		os.Exit(1)
	}

	var input []int
	for _, line := range strings.Split(strings.TrimSpace(string(file)), ",") {
		item, _ := strconv.Atoi(line)
		input = append(input, item)
	}
	return input
}
