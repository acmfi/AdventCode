package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

var alarm []int = nil
var opcode int = 19690720

func main() {
	// Part 1
	readFile()
	fmt.Println("Original IntCode\n", alarm)
	fmt.Println("\nPart 1 -- Alarm[0] stores", calcOpCode(alarm, 12, 2))
	// Part 2
	readFile()
	fmt.Println("\nResult IntCode\n", alarm)
	fmt.Println("\nPart 2 -- Result Code:", findOpCode(alarm, opcode))
	fmt.Println("\nResult IntCode\n", alarm)
}

func findOpCode(alarm []int, opcode int) (res int) {
	for i := 0; i < 100; i++ {
		for j := 0; j < 100; j++ {
			if calcOpCode(alarm, i, j) == opcode {
				return 100*i + j
				fmt.Printf("OpCode %d --> Noun: %s, Verb: %s", i, j)
			}
		}
	}
	return 0
}

func calcOpCode(alarm []int, noun int, verb int) (pzero int) {
	alarm[1] = noun
	alarm[2] = verb
	for i := 0; i < len(alarm); i += 4 {
		if alarm[i] == 1 {
			alarm[alarm[i+3]] = alarm[alarm[i+1]] + alarm[alarm[i+2]]
		} else if alarm[i] == 2 {
			alarm[alarm[i+3]] = alarm[alarm[i+1]] * alarm[alarm[i+2]]
		} else {
			break
		}
	}
	return alarm[0]
}

func readFile() {
	fileBytes, _ := ioutil.ReadFile("input.txt")
	arr := strings.Split(string(fileBytes), ",")
	alarm = make([]int, len(arr))
	for i := range arr {
		alarm[i], _ = strconv.Atoi(arr[i])
	}
}
