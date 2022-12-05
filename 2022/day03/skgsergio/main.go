package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"

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

func toSet(items string) map[rune]struct{} {
	set := map[rune]struct{}{}

	for _, chr := range items {
		set[chr] = struct{}{}
	}

	return set
}

func priority(item rune) int {
	if item >= 'a' {
		return int(1 + item - 'a')
	}

	return int(27 + item - 'A')
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	current := 0
	group := [3]map[rune]struct{}{}

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		s := scanner.Text()

		// Part 1
		c1 := toSet(s[:len(s)/2])
		c2 := toSet(s[len(s)/2:])

		var inBoth rune

		for item := range c1 {
			if _, ok := c2[item]; ok {
				inBoth = item
				break
			}
		}

		s1 += priority(inBoth)

		// Part 2
		group[current] = toSet(s)
		current = (current + 1) % 3

		if current == 0 {
			var inGroup rune

			for item := range group[0] {
				_, inTwo := group[1][item]
				_, inThree := group[2][item]

				if inTwo && inThree {
					inGroup = item
					break
				}
			}

			s2 += priority(inGroup)
		}
	}

	panicOnErr(scanner.Err())

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

	// Read file
	file, err := os.Open(*inputFile)
	panicOnErr(err)
	defer file.Close()

	// Solve
	s1, s2 := solve(file)
	fmt.Printf("Star 1: %d\n", s1)
	fmt.Printf("Star 2: %d\n", s2)
}
