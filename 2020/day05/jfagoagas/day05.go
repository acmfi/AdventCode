package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"sort"
)

func main() {
	input := readLines(os.Args[1])
	var seats []int
	for index := range input {
		rowMax := 128
		rowMin := 0
		rows := input[index][:7]
		columnMax := 8
		columnMin := 0
		columns := input[index][7:]
		for i := 0; i < len(rows); i++ {
			value := string(rows[i])
			if value == "F" {
				rowMax = rowMax - ((rowMax / 2) - (rowMin / 2))
			}
			if value == "B" {
				rowMin = rowMin + ((rowMax / 2) - (rowMin / 2))
			}
		}
		for i := 0; i < len(columns); i++ {
			value := string(columns[i])
			if value == "L" {
				columnMax = columnMax - ((columnMax / 2) - (columnMin / 2))
			}
			if value == "R" {
				columnMin = columnMin + ((columnMax / 2) - (columnMin / 2))
			}
		}
		row := math.Min(float64(rowMax), float64(rowMin))
		column := math.Min(float64(columnMax), float64(columnMin))
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
