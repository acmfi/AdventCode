package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func neededFuel(mass uint64) uint64 {
	// equation is: (mass / 3) - 2
	ans := (mass / 3)
	if ans >= 2 {
		return ans - 2
	}
	// negative fuel is zero fuel
	return 0
}

func main() {
	file, err := os.Open("input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	var totalFuel uint64
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		mass, err := strconv.ParseUint(scanner.Text(), 10, 64)
		if err != nil {
			log.Fatalf("could not parse line: %v", err)
		}
		fuel := neededFuel(mass)
		for fuel > 0 {
			totalFuel += fuel
			fuel = neededFuel(fuel)
		}
	}
	fmt.Println(totalFuel)
}
