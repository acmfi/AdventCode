package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func readLines(path string) []int {
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

	var lines []int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line, _ := strconv.Atoi(scanner.Text())
		lines = append(lines, line)
	}
	return lines
}
