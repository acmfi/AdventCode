package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type instructionSet struct {
	code         string
	codeoperator string
	codevalue    int
	executed     bool
}

func main() {

	file, _ := ioutil.ReadFile(os.Args[1])
	input := strings.Split((string(file)), "\n")

	fmt.Printf("Day 8\nPart 1: Accumulator = %d\n", checkLoop(input))
}

func checkLoop(input []string) int {
	// bootCode contains as key the instruction index and their values as keys
	bootCode := make(map[int]instructionSet)
	accumulator := 0
	for index := 0; index < len(input); index++ {
		// Instruction elements
		instruction := strings.Split(input[index], " ")
		operation := instruction[0]
		argument := instruction[1]
		operator := string(argument[0])
		value, _ := strconv.Atoi(string(argument[1:]))
		// Store instruction
		var ins instructionSet
		if _, ok := bootCode[index]; !ok {
			ins = instructionSet{
				code:         operation,
				codeoperator: operator,
				codevalue:    value,
				executed:     false,
			}
			bootCode[index] = ins
		}
		// If each instruction has not been executed
		if !bootCode[index].executed {
			//fmt.Println("Index:", index, "--> Operation:", operation, "-- Argument:", argument)
			switch operation {
			case "nop":
				// Nothing to do
			case "acc":
				switch operator {
				case "+":
					accumulator = accumulator + value
				case "-":
					accumulator = accumulator - value
				}
			case "jmp":
				switch operator {
				case "+":
					index = index + value - 1
				case "-":
					index = index - value - 1
				}
			}
			// Update exection status
			ins.executed = true
			bootCode[index] = ins
		} else {
			break
		}
	}
	return accumulator
}
