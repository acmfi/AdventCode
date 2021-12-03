package main

import (
	"flag"
	"fmt"
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

func bitCount(diags []string, bit int) (uint, uint) {
	var ones uint = 0
	var zeros uint = 0

	for _, diag := range diags {
		if diag[bit] == '1' {
			ones++
		} else {
			zeros++
		}
	}

	return ones, zeros
}

func star1(diags []string) uint64 {
	gammaStr := ""
	epsilonStr := ""

	for bit := 0; bit < len(diags[0]); bit++ {
		ones, zeros := bitCount(diags, bit)

		if ones >= zeros {
			gammaStr += "1"
			epsilonStr += "0"
		} else {
			gammaStr += "0"
			epsilonStr += "1"
		}
	}

	// Convert bits strings to uint64
	gamma, err := strconv.ParseUint(gammaStr, 2, 64)
	panicOnErr(err)
	epsilon, err := strconv.ParseUint(epsilonStr, 2, 64)
	panicOnErr(err)

	return gamma * epsilon
}

func filter(diags []string, bit int, bitCriteria func(uint, uint) byte) []string {
	// If there is just one diagnostic we do not need filtering
	if len(diags) == 1 {
		return diags
	}

	// Find the wanted bit in base to the ones and zeros according to a given criteria
	ones, zeros := bitCount(diags, bit)
	wantedBit := bitCriteria(ones, zeros)

	// Keep only the diagnostics with the matching bit value
	newDiags := []string{}

	for _, diag := range diags {
		if diag[bit] == wantedBit {
			newDiags = append(newDiags, diag)
		}
	}

	return newDiags
}

func star2(diags []string) uint64 {
	o2 := diags
	co2 := diags

	for bit := 0; bit < len(diags[0]); bit++ {
		// O2 Generator Rating
		o2 = filter(o2, bit, func(ones uint, zeros uint) byte {
			if ones < zeros {
				return '0'
			}
			return '1'
		})

		// CO2 Scrubber Rating
		co2 = filter(co2, bit, func(ones uint, zeros uint) byte {
			if ones < zeros {
				return '1'
			}
			return '0'
		})
	}

	// Convert bits strings to uint64
	o2gr, err := strconv.ParseUint(o2[0], 2, 64)
	panicOnErr(err)
	co2sr, err := strconv.ParseUint(co2[0], 2, 64)
	panicOnErr(err)

	return o2gr * co2sr
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
	diagnostics := strings.Fields(string(input))

	// Solve
	fmt.Printf("Star 1: %d\n", star1(diagnostics))
	fmt.Printf("Star 2: %d\n", star2(diagnostics))
}
