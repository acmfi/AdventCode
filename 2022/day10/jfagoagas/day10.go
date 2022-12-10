package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
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

func readLines(path string) []string {
	file, err := os.Open(path)
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()

	scanner := bufio.NewScanner(file)

	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type instruction struct {
	operation string
	value     int
}

var cycleSignalStrength map[int]int = map[int]int{
	20:  0,
	60:  0,
	100: 0,
	140: 0,
	180: 0,
	220: 0,
}

func parseInstructions(input []string) []instruction {
	instructions := []instruction{}
	for _, line := range input {
		values := strings.Split(line, " ")
		operation := values[0]
		value := 0
		var err error
		// Extract the value for the "addx"
		if len(values) == 2 {
			value, err = strconv.Atoi(values[1])
			exitOnError(err)
		}
		instructions = append(instructions, instruction{
			operation: operation,
			value:     value,
		})
	}
	return instructions
}

type sprite struct {
	left   int
	middle int
	rigth  int
}

func execInstructions(instructions []instruction) (int, string) {
	// Part 1
	signalStrength := 0

	// Start setting the same number of cycles than the instructions
	cycles := len(instructions)
	// X register starts with 1
	x := 1
	// PC points to the first instruction (index 0)
	pc := 0

	// Cycle - Value Queue
	addQueue := make(map[int]int)
	// To perform operations without reading instructions
	skipNext := false

	// Part 2
	image := ""
	// Print pointer
	position := 0
	// Max Wide
	maxWide := 40
	// Set sprite initial position
	sprite := sprite{
		left:   0,
		middle: 1,
		rigth:  2,
	}

	// Loop over the cycles
	for i := 0; i < cycles; i++ {
		// Set actual cycle
		cycle := i + 1

		// fmt.Printf("Starting cycle %d -- X = %d\n", cycle, x)

		// START PART 2
		// fmt.Printf("Sprite: %+v\n", sprite)
		// fmt.Printf("Position: %d\n", position)
		// If we reach the end of the line
		// we have to reset the printer
		if position == maxWide {
			// lines = append(lines, line)
			position = 0
			image += "\n"
			// fmt.Println()
		}

		if position == sprite.left || position == sprite.middle || position == sprite.rigth {
			image += "█"
		} else {
			image += " "
		}
		// fmt.Println(image)
		position++
		// END PART 2

		// Retrieve operation and value
		operation := ""
		value := 0
		if !skipNext {
			operation = instructions[pc].operation
			value = instructions[pc].value
		}
		// Part1
		if _, ok := cycleSignalStrength[cycle]; ok {
			cycleSignalStrength[cycle] = cycle * x
			signalStrength += cycle * x
		}

		// fmt.Printf("Cycle: %d -- PC: %d [%+v]\n", cycle, pc, instructions[pc])
		switch operation {
		case "addx":
			// Increase one cycle
			cycles++
			// Include the value to add and in the next cycle
			addQueue[cycle+1] = value
			// Skip next cycle since we need
			// to perform the operation
			skipNext = true

		case "noop":
			// Do not increse cycles
			// so next cycle reads an operation
			skipNext = false
			// Advance to the next execution
			pc++

		default:
			// Nothing to do here
		}

		// Check the values to add to the X registes
		if val, ok := addQueue[cycle]; ok {
			// Increase X register
			x += val
			// Delete this operation
			delete(addQueue, cycle)
			// Advance the PC
			pc++
			// Do not increse cycles
			// so next cycle reads an operation
			skipNext = false

			// Part 2 - Increase the sprite
			sprite.left = x - 1
			sprite.middle = x
			sprite.rigth = x + 1
		}
		// fmt.Printf("Finishing cycle %d -- X = %d\n\n", cycle, x)
	}

	return signalStrength, image
}

func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)
	// Parse instructions
	instructions := parseInstructions(input)

	// Solve
	part1, image := execInstructions(instructions)

	fmt.Printf("Part 1 - Result: %d\n", part1)
	fmt.Printf("Part 2 - Result: \n%s\n\n", image)
}
