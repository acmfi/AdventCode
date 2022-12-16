package main

import (
	"bufio"
	"flag"
	"fmt"
	"math"
	"os"
	"regexp"
	"sort"
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

func sliceContains[T comparable](where []T, what T) bool {
	for _, e := range where {
		if what == e {
			return true
		}
	}
	return false
}

func sliceIntersects[T comparable](s1 []T, s2 []T) bool {
	for _, e := range s1 {
		if sliceContains(s2, e) {
			return true
		}
	}
	return false
}

type Valve struct {
	flowRate int
	tunnels  []string
}

// https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
func calcDistances(valves map[string]Valve) map[string]map[string]int {
	dist := map[string]map[string]int{}

	for v := range valves {
		dist[v] = map[string]int{}

		for u := range valves {
			dist[v][u] = math.MaxInt32
		}
	}

	for v := range valves {
		dist[v][v] = 0

		for _, u := range valves[v].tunnels {
			dist[v][u] = 1
		}
	}

	for k := range valves {
		for i := range valves {
			for j := range valves {
				d := dist[i][k] + dist[k][j]

				if d < dist[i][j] {
					dist[i][j] = d
				}
			}
		}
	}

	return dist
}

func calcPressure(
	valves map[string]Valve, distances map[string]map[string]int,
	openOrder []string, timeRemaining int,
) int {
	current := "AA"
	pressure := 0

	for _, next := range openOrder {
		timeRemaining -= distances[current][next] + 1
		pressure += valves[next].flowRate * timeRemaining
		current = next
	}

	return pressure
}

func genOpenCombinations(
	distances map[string]map[string]int, flowing map[string]struct{},
	current string, open []string, timeRemaining int,
) [][]string {
	combinations := [][]string{}

	for next := range flowing {
		if !sliceContains(open, next) && distances[current][next] <= timeRemaining {
			open = append(open, next)
			combinations = append(
				combinations,
				genOpenCombinations(distances, flowing, next, open, timeRemaining-distances[current][next]-1)...,
			)
			open = open[:len(open)-1]
		}
	}

	if len(open) != 0 {
		combination := make([]string, len(open))
		copy(combination, open)
		combinations = append(combinations, combination)
	}

	return combinations
}

func part1(valves map[string]Valve, distances map[string]map[string]int, flowing map[string]struct{}) int {
	maxPressure := 0

	for _, combination := range genOpenCombinations(distances, flowing, "AA", []string{}, 30) {
		pressure := calcPressure(valves, distances, combination, 30)
		if pressure > maxPressure {
			maxPressure = pressure
		}
	}

	return maxPressure
}

func part2(valves map[string]Valve, distances map[string]map[string]int, flowing map[string]struct{}) int {
	maxPressure := 0

	setPressures := map[string]int{}
	for _, combination := range genOpenCombinations(distances, flowing, "AA", []string{}, 26) {
		pressure := calcPressure(valves, distances, combination, 26)

		sort.Strings(combination)

		k := strings.Join(combination, ",")
		if pressure > setPressures[k] {
			setPressures[k] = pressure
		}
	}

	opens := [][]string{}
	pressures := []int{}
	for k, v := range setPressures {
		opens = append(opens, strings.Split(k, ","))
		pressures = append(pressures, v)
	}

	for human := 0; human < len(setPressures); human++ {
		for elephant := human + 1; elephant < len(setPressures); elephant++ {
			if sliceIntersects(opens[human], opens[elephant]) {
				continue
			}

			s := pressures[human] + pressures[elephant]

			if s > maxPressure {
				maxPressure = s
			}
		}
	}

	return maxPressure
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	valves := map[string]Valve{}

	inputRe := regexp.MustCompile(
		`(?m)Valve (?P<name>\w+) has flow rate=(?P<flowRate>\d+); tunnels? leads? to valves? (?P<tunnels>[\w, ]+)`,
	)
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		var err error

		valveName := ""
		valve := Valve{
			flowRate: 0,
			tunnels:  []string{},
		}

		match := inputRe.FindStringSubmatch(scanner.Text())
		for i, name := range inputRe.SubexpNames() {
			switch name {
			case "name":
				valveName = match[i]
			case "flowRate":
				valve.flowRate, err = strconv.Atoi(match[i])
				panicOnErr(err)
			case "tunnels":
				valve.tunnels = strings.Split(match[i], ", ")
			}
		}

		valves[valveName] = valve
	}
	panicOnErr(scanner.Err())

	open := make(map[string]struct{}, len(valves))
	for k, v := range valves {
		if v.flowRate > 0 {
			open[k] = struct{}{}
		}
	}

	distances := calcDistances(valves)

	flowing := map[string]struct{}{}
	for k, v := range valves {
		if v.flowRate > 0 {
			flowing[k] = struct{}{}
		}
	}

	// Part 1
	s1 = part1(valves, distances, flowing)

	// Part 2
	s2 = part2(valves, distances, flowing)

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
