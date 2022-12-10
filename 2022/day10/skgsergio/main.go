package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"

	"github.com/pkg/profile"
)

var (
	inputFile = flag.String("f", "input.txt", "puzzle input file")
	profiling = flag.String("profile", "", "enable profiler, specify 'cpu' or 'mem'")
)

func panicOnErr(err error) {
	if err != nil {
		panic(err)
	}
}

func solve(input *os.File) (int, string) {
	s1 := 0
	s2 := ""

	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	instructions := strings.Split(strings.TrimSpace(string(inputBytes)), "\n")

	pc := 0
	cycle := 1
	xReg := 1

	addxRdy := false
	addxVal := 0

	for pc < len(instructions) || addxRdy {
		// Part 2
		xPos := (cycle - 1) % 40

		if xPos == 0 {
			s2 += "\n"
		}

		if xReg-1 <= xPos && xPos <= xReg+1 {
			s2 += "█"
		} else {
			s2 += " "
		}

		// Part 1
		if cycle == 20 || (cycle-20)%40 == 0 {
			s1 += xReg * cycle
		}

		if addxRdy {
			xReg += addxVal
			addxRdy = false
		} else {
			instr := strings.Split(instructions[pc], " ")
			pc += 1

			if instr[0] == "addx" {
				num, err := strconv.Atoi(instr[1])
				panicOnErr(err)

				addxVal = num
				addxRdy = true
			}
		}

		cycle += 1
	}

	return s1, s2
}

func main() {
	flag.Parse()

	// Profiler
	switch *profiling {
	case "cpu":
		defer profile.Start(profile.CPUProfile, profile.ProfilePath(".")).Stop()
	case "mem":
		defer profile.Start(profile.MemProfile, profile.ProfilePath(".")).Stop()
	}

	// Open file
	file, err := os.Open(*inputFile)
	panicOnErr(err)
	defer file.Close()

	// Solve
	s1, s2 := solve(file)
	fmt.Printf("Star 1: %v\n", s1)
	fmt.Printf("Star 2: %v\n", s2)
}
