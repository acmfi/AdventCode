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
	fmt.Printf("Day 10\nPart1: %d\n", difference1*difference3)
}
