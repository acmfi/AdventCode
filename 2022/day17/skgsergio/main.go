package main

import (
	"flag"
	"fmt"
	"io/ioutil"
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

type Pair struct {
	x, y int
}

func (p Pair) Add(q Pair) Pair {
	return Pair{p.x + q.x, p.y + q.y}
}

func movement(tower map[Pair]struct{}, rock []Pair, pos Pair) bool {
	for _, r := range rock {
		p := pos.Add(r)
		_, exists := tower[p]
		if p.x < 0 || p.x > 6 || p.y <= 0 || exists {
			return false
		}
	}

	return true
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	jets := strings.TrimSpace(string(inputBytes))

	jetMovement := map[byte]int{'>': 1, '<': -1}

	rocks := [][]Pair{
		{{0, 0}, {1, 0}, {2, 0}, {3, 0}},
		{{1, 0}, {0, 1}, {2, 1}, {1, 2}},
		{{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}},
		{{0, 0}, {0, 1}, {0, 2}, {0, 3}},
		{{0, 0}, {1, 0}, {0, 1}, {1, 1}},
	}

	maxRocks := 1_000_000_000_000
	tower := map[Pair]struct{}{{-1, 0}: {}}
	visitedStates := map[Pair]Pair{}
	rockIdx := 0
	jetIdx := 0

	for rockN := 0; rockN < maxRocks; rockN++ {
		height := 0
		for p := range tower {
			if p.y > height {
				height = p.y
			}
		}

		// Part 1
		if rockN == 2022 {
			s1 = height
		}

		pos := Pair{2, height + 4}

		state := Pair{rockIdx, jetIdx}
		if stateRocksHeight, ok := visitedStates[state]; ok {
			if (maxRocks-rockN)%(stateRocksHeight.x-rockN) == 0 {
				s2 = height + ((stateRocksHeight.y - height) * ((maxRocks - rockN) / (stateRocksHeight.x - rockN)))
				break
			}
		} else {
			visitedStates[state] = Pair{rockN, height}
		}

		for {
			// Side movement
			sidePos := pos.Add(Pair{jetMovement[jets[jetIdx]], 0})

			if movement(tower, rocks[rockIdx], sidePos) {
				pos = sidePos
			}

			jetIdx = (jetIdx + 1) % len(jets)

			// Downward movement
			downPos := pos.Add(Pair{0, -1})

			if movement(tower, rocks[rockIdx], downPos) {
				pos = downPos
			} else {
				break
			}
		}

		for _, r := range rocks[rockIdx] {
			tower[pos.Add(r)] = struct{}{}
		}

		rockIdx = (rockIdx + 1) % len(rocks)
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
