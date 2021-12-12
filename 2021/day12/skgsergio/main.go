package main

import (
	"flag"
	"fmt"
	"os"
	"strings"
	"unicode"

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

type Cave string

func (c *Cave) IsSmall() bool {
	return unicode.IsLower(rune((*c)[0]))
}

func findAllPaths(caves map[Cave][]Cave, smallVisited map[Cave]bool, current Cave, allowSmallDoubleVisit bool) int {
	if current.IsSmall() {
		if smallVisited[current] {
			allowSmallDoubleVisit = false
		}
		smallVisited[current] = true
	}

	pathCount := 0

	for _, next := range caves[current] {
		// If its the starting cave skip it
		if next == "start" {
			continue
		}

		// If its the ending cave sum one path to the path count and continue
		if next == "end" {
			pathCount++
			continue
		}

		// If is an small cave and its already visited and a doble visit
		// is not allowed, skip it.
		if next.IsSmall() && smallVisited[next] && !allowSmallDoubleVisit {
			continue
		}

		smallVisitedNext := smallVisited

		// Only copy the small visited list if the next is an small cave
		// this way we avid copying it always, which is costly.
		if next.IsSmall() {
			smallVisitedNext = make(map[Cave]bool, len(smallVisited))
			for k, v := range smallVisited {
				smallVisitedNext[k] = v
			}
		}

		pathCount += findAllPaths(caves, smallVisitedNext, next, allowSmallDoubleVisit)
	}

	return pathCount
}

func star1(caves map[Cave][]Cave) int {
	return findAllPaths(caves, map[Cave]bool{}, "start", false)
}

func star2(caves map[Cave][]Cave) int {
	return findAllPaths(caves, map[Cave]bool{}, "start", true)
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
	caves := map[Cave][]Cave{}

	for _, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		path := strings.Split(line, "-")
		from := Cave(strings.TrimSpace(path[0]))
		to := Cave(strings.TrimSpace(path[1]))

		caves[from] = append(caves[from], to)
		caves[to] = append(caves[to], from)
	}

	// Solve
	fmt.Printf("Star 1: %d\n", star1(caves))
	fmt.Printf("Star 2: %d\n", star2(caves))
}
