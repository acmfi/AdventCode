package main

import (
	"flag"
	"fmt"
	"io/ioutil"
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

type Pair [2]int

func index[T comparable](s []T, v T) int {
	for i := range s {
		if s[i] == v {
			return i
		}
	}
	return -1
}

func insert[T any](s []T, index int, v T) []T {
	if len(s) == index {
		return append(s, v)
	}

	s = append(s[:index+1], s[index:]...)
	s[index] = v
	return s
}

func mixFile(file []Pair, iterations int) []Pair {
	fileLen := len(file)
	fileMix := make([]Pair, fileLen)
	copy(fileMix, file)

	for it := 0; it < iterations; it++ {
		for _, n := range file {
			old_position := index(fileMix, n)
			fileMix = append(fileMix[:old_position], fileMix[old_position+1:]...)

			new_position := (old_position + n[1]) % (fileLen - 1)
			if new_position <= 0 {
				new_position += (fileLen - 1)
			}

			fileMix = insert(fileMix, new_position, n)
		}
	}

	return fileMix
}

func grooveSum(file []Pair, key int, iterations int) int {
	fileLen := len(file)
	fileMod := []Pair{}
	for _, pair := range file {
		fileMod = append(fileMod, Pair{pair[0], pair[1] * key})
	}

	result := mixFile(fileMod, iterations)

	zeroIdx := 0
	for idx, n := range result {
		if n[1] == 0 {
			zeroIdx = idx
			break
		}
	}

	return result[(zeroIdx+1000)%fileLen][1] + result[(zeroIdx+2000)%fileLen][1] + result[(zeroIdx+3000)%fileLen][1]
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	file := []Pair{}
	for idx, nStr := range strings.Split(strings.TrimSpace(string(inputBytes)), "\n") {
		n, err := strconv.Atoi(nStr)
		panicOnErr(err)

		file = append(file, Pair{idx, n})
	}

	s1 = grooveSum(file, 1, 1)
	s2 = grooveSum(file, 811589153, 10)

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
