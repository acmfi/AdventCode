package main

import (
	"bufio"
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

type Section struct {
	start int
	end   int
}

func (s *Section) Contains(o Section) bool {
	return s.start <= o.start && o.end <= s.end
}

func (s *Section) Overlaps(o Section) bool {
	return s.start <= o.end && s.end >= o.start
}

type Pair struct {
	section1 Section
	section2 Section
}

func (p *Pair) Contained() bool {
	return p.section1.Contains(p.section2) || p.section2.Contains(p.section1)
}

func (p *Pair) Overlapped() bool {
	return p.section1.Overlaps(p.section2)
}

func ParsePair(line string) Pair {
	pairSplit := strings.Split(line, ",")
	if len(pairSplit) != 2 {
		panicOnErr(fmt.Errorf("Expected format X-Y,Z-W but got: %s", line))
	}

	sections := [2]Section{}
	for idx := range sections {
		sectionSplit := strings.Split(pairSplit[idx], "-")
		if len(sectionSplit) != 2 {
			panicOnErr(fmt.Errorf("Expected format X-Y but got: %s", pairSplit[idx]))
		}

		start, err := strconv.Atoi(sectionSplit[0])
		panicOnErr(err)
		sections[idx].start = start

		end, err := strconv.Atoi(sectionSplit[1])
		panicOnErr(err)
		sections[idx].end = end
	}

	return Pair{sections[0], sections[1]}
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		s := scanner.Text()

		pair := ParsePair(s)

		if pair.Contained() {
			s1 += 1
		}

		if pair.Overlapped() {
			s2 += 1
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
