package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func main() {
	// Part 1
	scanner := bufio.NewScanner(openInput())
	var total, fuel int = 0, 0
	for scanner.Scan() {
		mass, _ := strconv.Atoi(scanner.Text())
		fuel = calcFuel(mass)
		total += fuel
	}
	fmt.Println("Part 1 - Total fuel:", total)

	// Part 2
	scanner = bufio.NewScanner(openInput())
	total, fuel = 0, 0
	for scanner.Scan() {
		mass, _ := strconv.Atoi(scanner.Text())
		fuel = calcFuel(mass)
		for fuel > 0 {
			total += fuel
			fuel = calcFuel(fuel)
		}
	}
	fmt.Println("Part 2 - Total fuel:", total)
}

func openInput() (file *os.File) {
	input := os.Args[1]
	file, err := os.Open(input)
	if err != nil {
		os.Exit(1)
	}
	return
}

func calcFuel(mass int) (fuel int) {
	fuel = (mass / 3) - 2
	return
}
