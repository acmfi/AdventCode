package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	input := readLines()
	fmt.Printf("Part 1 - Result: %d\n", part1(input))
	fmt.Printf("Part 1 - Result: %d\n", part2(input))
}

func part1(input []string) int {
	horizontalPosition := 0
	depthPosition := 0
	for _, value := range input {
		instruction := strings.Split(value, " ")
		operation := instruction[0]
		counter, err := strconv.Atoi(instruction[1])
		if err != nil {
			log.Panic(err)
		}
		switch operation {
		case "forward":
			horizontalPosition += counter
		case "up":
			depthPosition -= counter
		case "down":
			depthPosition += counter
		}
	}
	return horizontalPosition * depthPosition
}

func part2(input []string) int {
	horizontalPosition := 0
	depthPosition := 0
	aimPosition := 0
	for _, value := range input {
		instruction := strings.Split(value, " ")
		operation := instruction[0]
		counter, err := strconv.Atoi(instruction[1])
		if err != nil {
			log.Panic(err)
		}
		switch operation {
		case "forward":
			horizontalPosition += counter
			depthPosition += aimPosition * counter
		case "up":
			aimPosition -= counter
		case "down":
			aimPosition += counter
		}
	}
	return horizontalPosition * depthPosition
}

func readLines() []string {
	file, err := os.Open(os.Args[1])
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

	var lines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}
