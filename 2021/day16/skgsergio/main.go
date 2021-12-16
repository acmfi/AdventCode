package main

import (
	"encoding/hex"
	"flag"
	"fmt"
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

func solve(b []byte) (int, int) {
	p, err := NewBITSParser(b).Parse()
	panicOnErr(err)
	return p.VersionSum(), p.Eval()
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
	bytestream, err := hex.DecodeString(strings.TrimSpace(string(input)))
	panicOnErr(err)

	// Solve
	star1, star2 := solve(bytestream)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
