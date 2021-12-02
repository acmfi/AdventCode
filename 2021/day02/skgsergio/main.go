package main

import (
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

type Action string

const (
	Forward Action = "forward"
	Down    Action = "down"
	Up      Action = "up"
)

type Command struct {
	Action Action
	Depth  int
}

func star1(cmds []Command) int {
	x, y := 0, 0

	for _, cmd := range cmds {
		switch cmd.Action {
		case Forward:
			x += cmd.Depth
		case Down:
			y += cmd.Depth
		case Up:
			y -= cmd.Depth
		}
	}

	return x * y
}

func star2(cmds []Command) int {
	x, y, aim := 0, 0, 0

	for _, cmd := range cmds {
		switch cmd.Action {
		case Forward:
			x += cmd.Depth
			y += aim * cmd.Depth
		case Down:
			aim += cmd.Depth
		case Up:
			aim -= cmd.Depth
		}
	}

	return x * y
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
	cmds := []Command{}
	for _, cmd := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		fields := strings.Fields(cmd)
		depth, err := strconv.Atoi(fields[1])
		panicOnErr(err)

		cmds = append(cmds, Command{Action(fields[0]), depth})
	}

	// Solve
	fmt.Printf("Star 1: %d\n", star1(cmds))
	fmt.Printf("Star 2: %d\n", star2(cmds))
}
