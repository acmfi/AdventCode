package main

import (
	"flag"
	"fmt"
	"math"
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

type Element rune

const EdgeElement Element = ' '

func (e *Element) IsEdge() bool {
	return *e == EdgeElement
}

type Pair [2]Element

func (p *Pair) IsEdge() bool {
	return p[0].IsEdge() || p[1].IsEdge()
}

func polimerization(template string, rules map[Pair]Element, steps int) int {
	// Extract pairs from the template
	pairs := map[Pair]int{}
	for i := 0; i < len(template)-1; i++ {
		pairs[Pair{Element(template[i]), Element(template[i+1])}]++
	}

	// Insert edge + first element and last + edge pairs to avoid having to do
	// extra calcs with math.Ceil and shit later for getting the correct result.
	pairs[Pair{EdgeElement, Element(template[0])}]++
	pairs[Pair{Element(template[len(template)-1]), EdgeElement}]++

	// Perform the polimerization by replacing pairs according to the rules.
	for i := 0; i < steps; i++ {
		stepPairs := map[Pair]int{}

		for pair, count := range pairs {
			if pair.IsEdge() {
				stepPairs[pair] += count
				continue
			}

			stepPairs[Pair{pair[0], rules[pair]}] += count
			stepPairs[Pair{rules[pair], pair[1]}] += count
		}

		pairs = stepPairs
	}

	// Count the number of individual elements extracting them from the pairs,
	// and find the max and min counts.
	counts := map[Element]int{}
	for pair, count := range pairs {
		counts[pair[0]] += count
		counts[pair[1]] += count
	}

	max, min := math.MinInt, math.MaxInt
	for element, count := range counts {
		if element.IsEdge() {
			continue
		}

		// As Pairs overlap we need to divide the values
		// by 2 as we are counting elements twice.
		count /= 2

		if count < min {
			min = count
		}

		if count > max {
			max = count
		}
	}

	return max - min
}

func solve(template string, rules map[Pair]Element) (int, int) {
	return polimerization(template, rules, 10), polimerization(template, rules, 40)
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
	template := ""
	rules := map[Pair]Element{}

	for i, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		line = strings.TrimSpace(line)

		if i == 0 {
			template = line
			continue
		}

		if line == "" {
			continue
		}

		rule := strings.Split(line, " -> ")
		rules[Pair{Element(rule[0][0]), Element(rule[0][1])}] = Element(rule[1][0])
	}

	// Solve
	star1, star2 := solve(template, rules)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
