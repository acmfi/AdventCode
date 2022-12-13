package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"sort"
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

func convert(packet any) ([]any, bool) {
	switch packet.(type) {
	case []any, []float64:
		return packet.([]any), false
	case float64:
		return []any{packet}, true
	default:
		panicOnErr(fmt.Errorf("unknown conversion for type '%T'", packet))
	}
	return []any{}, false
}

func compare(packet1 any, packet2 any) int {
	p1, p1IsNum := convert(packet1)
	p2, p2IsNum := convert(packet2)

	if p1IsNum && p2IsNum {
		return int(p1[0].(float64)) - int(p2[0].(float64))
	}

	for idx := range p1 {
		if len(p2) <= idx {
			return 1
		}

		c := compare(p1[idx], p2[idx])
		if c != 0 {
			return c
		}
	}

	if len(p1) == len(p2) {
		return 0
	}

	return -1
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	packets := []any{}

	for idx, p := range strings.Split(strings.TrimSpace(string(inputBytes)), "\n\n") {
		packet := [2]any{}

		pair := strings.Split(p, "\n")

		err := json.Unmarshal([]byte(pair[0]), &packet[0])
		panicOnErr(err)
		packets = append(packets, packet[0])

		err = json.Unmarshal([]byte(pair[1]), &packet[1])
		panicOnErr(err)
		packets = append(packets, packet[1])

		if compare(packet[0], packet[1]) <= 0 {
			s1 += idx + 1
		}
	}

	// Part 2
	div1 := []any{[]any{float64(2)}}
	packets = append(packets, div1)
	div2 := []any{[]any{float64(6)}}
	packets = append(packets, div2)

	sort.Slice(packets, func(i, j int) bool { return compare(packets[i], packets[j]) < 0 })

	s2 = 1
	for idx, p := range packets {
		if compare(p, div1) == 0 || compare(p, div2) == 0 {
			s2 *= idx + 1
		}
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
