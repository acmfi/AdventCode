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

	// Part 1
	_, accumulator1 := checkLoop(input)
	// Part 2
	accumulator2 := changeJMPNOP(input)
	fmt.Printf("Day 8\nPart 1: Accumulator = %d\nPart 2: Accumulator = %d\n", accumulator1, accumulator2)
}

func changeJMPNOP(input []string) int {
	var testCode [][]string
	testCode = append(testCode, input)
	for index := range input {
		instruction := strings.Split(input[index], " ")
		operation := instruction[0]
		argument := instruction[1]

		if operation == "nop" {
			tmp := make([]string, len(input))
			copy(tmp, input)
			tmp[index] = "jmp " + argument
			testCode = append(testCode, tmp)
		}
		if operation == "jmp" {
			tmp := make([]string, len(input))
			copy(tmp, input)
			tmp[index] = "nop " + argument
			testCode = append(testCode, tmp)
		}
	}
	var accumulator2 int
	for i := range testCode {
		infiniteLoop, accumulator := checkLoop(testCode[i])
		if !infiniteLoop {
			accumulator2 = accumulator
			break
		}
	}
	return accumulator2
}

func checkLoop(input []string) (bool, int) {
	// bootCode contains as key the instruction index and their values as keys
	bootCode := make(map[int]instructionSet)
	accumulator := 0
	infiniteLoop := false
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
			infiniteLoop = true
			//fmt.Println("infiniteLoop", accumulator)
			break
		}
	}
	return infiniteLoop, accumulator
}
