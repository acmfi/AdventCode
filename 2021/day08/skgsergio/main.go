package main

import (
	"flag"
	"fmt"
	"math/bits"
	"os"
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

type Pattern string

func (p Pattern) ToUint() uint {
	res := uint(0)
	for i := range p {
		res |= 1 << (p[i] - 'a')
	}
	return res
}

type Signal struct {
	inputs  []Pattern
	outputs []Pattern
}

func decodeSignal(signal Signal) int {
	patternDigit := map[uint]int{}

	// Known values
	one := uint(0)
	four := uint(0)
	for _, pattern := range signal.inputs {
		pval := pattern.ToUint()

		switch len(pattern) {
		case 2:
			one = pval
			patternDigit[pval] = 1
		case 3:
			patternDigit[pval] = 7
		case 4:
			four = pval
			patternDigit[pval] = 4
		case 7:
			patternDigit[pval] = 8
		}
	}

	// Decode remaining values
	for _, pattern := range signal.inputs {
		pval := pattern.ToUint()

		switch len(pattern) {
		case 5:
			if bits.OnesCount(pval&one) == 2 {
				patternDigit[pval] = 3
			} else if bits.OnesCount(pval&four) == 2 {
				patternDigit[pval] = 2
			} else {
				patternDigit[pval] = 5
			}
		case 6:
			if bits.OnesCount(pval&four) == 4 {
				patternDigit[pval] = 9
			} else if bits.OnesCount(pval&one) == 2 {
				patternDigit[pval] = 0
			} else {
				patternDigit[pval] = 6
			}
		}
	}

	// Decode number
	number := 0

	for _, pattern := range signal.outputs {
		number *= 10
		number += patternDigit[pattern.ToUint()]
	}

	return number
}

func star2(signals []Signal) int {
	sum := 0
	for _, signal := range signals {
		sum += decodeSignal(signal)
	}
	return sum
}

func star1(signals []Signal) int {
	count := 0

	for _, signal := range signals {
		for _, out := range signal.outputs {
			if len(out) == 2 || len(out) == 3 || len(out) == 4 || len(out) == 7 {
				count++
			}
		}
	}

	return count
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

	// Read file
	input, err := os.ReadFile(*inputFile)
	panicOnErr(err)

	// Parse input
	signals := []Signal{}

	for _, entry := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		parts := strings.Split(entry, "|")

		inputs := []Pattern{}
		for _, p := range strings.Fields(strings.TrimSpace(parts[0])) {
			inputs = append(inputs, Pattern(p))
		}

		outputs := []Pattern{}
		for _, p := range strings.Fields(strings.TrimSpace(parts[1])) {
			outputs = append(outputs, Pattern(p))
		}

		signals = append(signals, Signal{inputs, outputs})
	}

	// Solve
	fmt.Printf("Star 1: %d\n", star1(signals))
	fmt.Printf("Star 2: %d\n", star2(signals))
}
