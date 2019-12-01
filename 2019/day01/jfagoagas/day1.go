package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	input1 := os.Args[1]
	file1, err := os.Open(input1)
	if err != nil {
		os.Exit(1)
	}
	defer file1.Close()

	// Part 1
	scanner1 := bufio.NewScanner(file1)
	var total, fuel int = 0, 0
	for scanner1.Scan() {
		mass, _ := strconv.Atoi(scanner1.Text())
		fuel = calcFuel(mass)
		total += fuel
	}
	fmt.Println("Part 1 - Total fuel:", total)

	input2 := os.Args[1]
	file2, err := os.Open(input2)
	if err != nil {
		os.Exit(1)
	}
	defer file2.Close()

	// Part 2
	scanner2 := bufio.NewScanner(file2)
	total, fuel = 0, 0
	for scanner2.Scan() {
		mass, _ := strconv.Atoi(scanner2.Text())
		fuel = calcFuel(mass)
		for fuel > 0 {
			total += fuel
			fuel = calcFuel(fuel)
		}
	}
	fmt.Println("Part 2 - Total fuel:", total)
}

func calcFuel(mass int) (fuel int) {
	fuel = (mass / 3) - 2
	return
}
