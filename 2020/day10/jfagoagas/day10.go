package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	file, _ := ioutil.ReadFile(os.Args[1])
	list := strings.Split((string(file)), "\n")
	var input []int
	for _, value := range list {
		item, _ := strconv.Atoi(value)
		input = append(input, item)
	}

	// Part 1
	sort.Ints(input)
	chargingOutlet := 0

	// Your device has a built-in joltage adapter rated for 3 jolts higher than the highest-rated adapter in your bag.
	deviceAdapter := input[len(input)-1] + 3
	input = append(input, deviceAdapter)

	difference1 := 0
	difference3 := 0
	for _, value := range input {
		difference := value - chargingOutlet
		if difference == 1 {
			difference1++
		} else {
			difference3++
		}
		//fmt.Printf("Joltage Differences -- Charging Outlet: %d -- Adapter: %d -- Difference: %d\n", chargingOutlet, value, difference)
		chargingOutlet = value
	}

	// Part 2
	accumulator := map[int]int{0: 1}
	for _, value := range input {
		//fmt.Println(value, accumulator[value-1], accumulator[value-2], accumulator[value-3])
		accumulator[value] = accumulator[value-1] + accumulator[value-2] + accumulator[value-3]
	}

	fmt.Printf("Day 10\nPart 1: %d\nPart 2: %d\n", difference1*difference3, accumulator[input[len(input)-1]])
}
