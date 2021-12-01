package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func star1(input []int) {
	cum := 0
	for i, num := range input {
		if i < len(input) - 1 && num < input[i+1] {
			cum++
		}
	}

	fmt.Printf("star 1: %d\n", cum)
}

func star2(input []int) {
	cum := 0
	for i := 1; i < len(input) - 2; i++ {
		if input[i] + input[i+1] + input[i+2] > input[i-1] + input[i] + input[i+1] {
			cum++
		}
	}

	fmt.Printf("star 2: %d\n", cum)
}

func main() {
	rawInput, err := os.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal(err)
		return
	}

	parsedInput:= []int{}
	for _, str := range strings.Fields(string(rawInput)) {
		convertedStr, err := strconv.Atoi(str)
		if err != nil {
			log.Fatal(err)
			return
		}
		
		parsedInput = append(parsedInput, convertedStr)
	}

	star1(parsedInput)
	star2(parsedInput)
}
