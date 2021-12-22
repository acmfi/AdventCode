package main

import (
	"flag"
	"fmt"
	"os"
	"regexp"
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

func maxInt(x, y int) int {
	if x > y {
		return x
	}
	return y
}

func minInt(x, y int) int {
	if x < y {
		return x
	}
	return y
}

type Point3D struct {
	x int
	y int
	z int
}

func (p *Point3D) Add(other Point3D) Point3D {
	return Point3D{p.x + other.x, p.y + other.y, p.z + other.z}
}

func (p *Point3D) Sub(other Point3D) Point3D {
	return Point3D{p.x - other.x, p.y - other.y, p.z - other.z}
}

type Step struct {
	from   Point3D
	to     Point3D
	status bool
}

func applyWindow(steps []Step, from Point3D, to Point3D) []Step {
	wsteps := []Step{}

	for _, c := range steps {
		if c.from.x >= to.x || c.to.x < from.x ||
			c.from.y >= to.y || c.to.y < from.y ||
			c.from.z >= to.z || c.to.z < from.z {
			continue
		}

		nc := Step{}

		nc.status = c.status

		nc.from.x = maxInt(c.from.x, from.x)
		nc.from.y = maxInt(c.from.y, from.y)
		nc.from.z = maxInt(c.from.z, from.z)

		nc.to.x = minInt(c.to.x, to.x)
		nc.to.y = minInt(c.to.y, to.y)
		nc.to.z = minInt(c.to.z, to.z)

		wsteps = append(wsteps, nc)
	}

	return wsteps
}

func count(step Step, rest []Step) int {
	overlaps := applyWindow(rest, step.from, step.to)

	total := (step.to.x - step.from.x + 1) * (step.to.y - step.from.y + 1) * (step.to.z - step.from.z + 1)

	for i, c := range overlaps {
		total -= count(c, overlaps[i+1:])
	}

	return total
}

func execute(steps []Step) int {
	total := 0

	for i, c := range steps {
		if !c.status {
			continue
		}

		total += count(c, steps[i+1:])
	}

	return total
}

func solve(steps []Step) (int, int) {
	return execute(applyWindow(steps, Point3D{-50, -50, -50}, Point3D{50, 50, 50})), execute(steps)
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
	steps := []Step{}

	re := regexp.MustCompile(`.*(?P<STATUS>on|off) x=(?P<MINX>-?\d+)..(?P<MAXX>-?\d+),y=(?P<MINY>-?\d+)..(?P<MAXY>-?\d+),z=(?P<MINZ>-?\d+)..(?P<MAXZ>-?\d+).*`)

	for _, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		step := Step{}

		match := re.FindStringSubmatch(line)
		for i, name := range re.SubexpNames() {
			if name == "" {
				continue
			}

			if name == "STATUS" {
				step.status = match[1] == "on"
				continue
			}

			c, err := strconv.Atoi(match[i])
			panicOnErr(err)

			switch name {
			case "MINX":
				step.from.x = c
			case "MAXX":
				step.to.x = c
			case "MINY":
				step.from.y = c
			case "MAXY":
				step.to.y = c
			case "MINZ":
				step.from.z = c
			case "MAXZ":
				step.to.z = c
			}
		}

		steps = append(steps, step)
	}

	// Solve
	star1, star2 := solve(steps)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
