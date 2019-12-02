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
	var total_p1, fuel1 = 0, 0
	for scanner.Scan() {
		mass, _ := strconv.Atoi(scanner.Text())
		fuel1 = calcFuel(mass)
		total_p1 += fuel1
	}
	fmt.Println("Part 1 - Total fuel:", total_p1)

	// Part 2
	scanner = bufio.NewScanner(openInput())
	var total_p2 = 0
	for scanner.Scan() {
		mass, _ := strconv.Atoi(scanner.Text())
		total_p2 += calcFuelRec(mass)
	}
	fmt.Println("Part 2 - Total fuel:", total_p2)
}

func openInput() (file *os.File) {
	file, err := os.Open("input.txt")
	if err != nil {
		os.Exit(1)
	}
	return
}

func calcFuel(mass int) (fuel int) {
	fuel = (mass / 3) - 2
	return
}

func calcFuelRec(mass int) (total int) {
	fuel := calcFuel(mass)
	for fuel > 0 {
		total += fuel
		fuel = calcFuel(fuel)
	}
	return
}
