package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	input := readLines(os.Args[1])
	var seats []int
	for _, entry := range input {
		replacer := strings.NewReplacer("F", "0", "B", "1", "L", "0", "R", "1")
		value := replacer.Replace(string(entry))
		row, _ := strconv.ParseInt(value[:7], 2, 64)
		column, _ := strconv.ParseInt(value[7:], 2, 64)
		seatID := int((row * 8) + column)
		seats = append(seats, seatID)
	}
	sort.Ints(seats)
	highestSeatID := 0
	mySeat := 0
	for i := 0; i < len(seats)-1; i++ {
		if seats[i] > highestSeatID {
			highestSeatID = seats[i]
		}
		seatDifference := seats[i+1] - seats[i]
		if seatDifference != 1 {
			mySeat = seats[i] + 1
		}

	}
	fmt.Printf("Day 5\nPart 1: %d\nPart 2: %d\n", highestSeatID, mySeat)
}

func readLines(path string) []string {
	file, err := os.Open(path)
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
		line := scanner.Text()
		lines = append(lines, line)
	}
	return lines
}
