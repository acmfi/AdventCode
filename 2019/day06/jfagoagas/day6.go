package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

var orbits []string = nil

func main() {
	readInput()
	//	fmt.Printf("%s\n", orbits)
	fmt.Printf("Part 1 -- Total orbits: %d\n", countOrbits(orbits))
}

func countOrbits(orbits []string) (c int) {
	c = 0
	var count = map[string]string{}
	for i := range orbits {
		s := strings.Split(orbits[i], ")")
		orbits := s[1]
		orbited := s[0]
		count[orbits] = orbited
	}

	for k := range count {
		for i, ok := count[k]; ok; i, ok = count[i] {
            fmt.Print("\n",count[k])
            fmt.Print("\n",count[i])
			c++
		}
	}

	return
}

func readInput() {
	input, _ := ioutil.ReadFile(os.Args[1])
	orbits = strings.Split(strings.TrimSpace(string(input)), "\n")
}
