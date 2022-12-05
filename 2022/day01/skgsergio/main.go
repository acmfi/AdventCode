package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"sort"
	"strconv"

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

func solve(input *os.File) (int, int) {
	elves := []int{0}
	idx := 0

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		s := scanner.Text()

		if len(s) == 0 {
			elves = append(elves, 0)
			idx += 1
			continue
		}

		i, err := strconv.Atoi(s)
		panicOnErr(err)
		elves[idx] += i
	}

	panicOnErr(scanner.Err())

	sort.Sort(sort.Reverse(sort.IntSlice(elves)))

	return elves[0], elves[0] + elves[1] + elves[2]
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
	file, err := os.Open(*inputFile)
	panicOnErr(err)
	defer file.Close()

	// Solve
	s1, s2 := solve(file)
	fmt.Printf("Star 1: %d\n", s1)
	fmt.Printf("Star 2: %d\n", s2)
}
