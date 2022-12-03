package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
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

func readLines(path string) [][]rune {
	file, err := os.Open(path)
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()

	scanner := bufio.NewScanner(file)

	var lines [][]rune
	for scanner.Scan() {
		if scanner.Text() == "" {
			continue
		}
		lines = append(lines, []rune(scanner.Text()))
	}
	return lines
}

type rucksack struct {
	// We are using map[rune]bool
	// to do lookups faster
	all          map[rune]bool
	compartment1 map[rune]bool
	compartment2 map[rune]bool
	common       rune
	priority     int
}

func parseRucksack(input [][]rune) []rucksack {
	var rucksacks []rucksack

	for _, line := range input {
		all := make(map[rune]bool)
		compartment1 := make(map[rune]bool)
		compartment2 := make(map[rune]bool)
		for index, char := range line {
			all[char] = true
			if index < len(line)/2 {
				compartment1[char] = true
			} else {
				compartment2[char] = true
			}
		}
		rucksacks = append(rucksacks, rucksack{compartment1: compartment1, compartment2: compartment2, all: all})
	}
	return rucksacks
}

func findCommonPriority(rucksacks []rucksack) []rucksack {
	for index, rucksack := range rucksacks {
		// Find the common item in both compartments
		for item1 := range rucksack.compartment1 {
			if ok := rucksack.compartment2[item1]; ok {
				rucksacks[index].common = item1
				rucksacks[index].priority = calcPriority(item1)
				break
			}
		}

	}
	return rucksacks
}

func calcPriority(char rune) int {
	priority := 0
	// lowercase, a (97) to z (122)
	if char <= 122 && char >= 97 {
		priority = int(char - 96)
	}
	// uppercase, A (65) to Z (90)
	if char <= 90 && char >= 65 {
		priority = int(char - 38)
	}
	return priority
}

func calcTotalPriority(rucksacks []rucksack) int {
	totalPriority := 0
	for _, rucksack := range rucksacks {
		totalPriority += rucksack.priority
	}
	return totalPriority
}

func calcBadgePriority(rucksacks []rucksack) int {
	priority := 0
	for i := 0; i < len(rucksacks); i += 3 {
		for item1 := range rucksacks[i].all {
			if rucksacks[i+1].all[item1] && rucksacks[i+2].all[item1] {
				priority += calcPriority(item1)
			}
		}
	}
	return priority
}

func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)
	// Parse Rucksacks
	rucksacks := parseRucksack(input)
	// Find common items and priority
	rucksacks = findCommonPriority(rucksacks)

	fmt.Printf("Part 1 - Result: %d\n", calcTotalPriority(rucksacks))
	fmt.Printf("Part 2 - Result: %d\n", calcBadgePriority(rucksacks))
}
