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

type Job struct {
	monkey1 string
	op      string
	monkey2 string
}

var OPS = map[string]func(int, int) int{
	"+": func(m1, m2 int) int { return m1 + m2 },
	"-": func(m1, m2 int) int { return m1 - m2 },
	"*": func(m1, m2 int) int { return m1 * m2 },
	"/": func(m1, m2 int) int { return m1 / m2 },
}

func resolve(jobs map[string]Job, results map[string]int, current string) int {
	if v, ok := results[current]; ok {
		return v
	}

	job, ok := jobs[current]
	if !ok {
		panicOnErr(fmt.Errorf("can't find operation for '%s'", current))
	}

	m1 := resolve(jobs, results, job.monkey1)
	m2 := resolve(jobs, results, job.monkey2)

	r := OPS[job.op](m1, m2)

	return r
}

func findHumn(jobs map[string]Job, results map[string]int) int {
	jobs["root"] = Job{
		monkey1: jobs["root"].monkey1,
		op:      "-",
		monkey2: jobs["root"].monkey2,
	}
	results["humn"] = 1

	limLow := 0
	limUp := 0
	for {
		res := resolve(jobs, results, "root")

		if res == 0 {
			return results["humn"]
		} else if res < 0 {
			limLow = results["humn"]
		} else {
			limUp = results["humn"]
		}

		if limLow == limUp {
			panicOnErr(fmt.Errorf("no solution found"))
		} else if limLow == 0 || limUp == 0 {
			results["humn"] *= 2
		} else {
			results["humn"] = (limLow + limUp) / 2
		}
	}
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	results := map[string]int{}
	jobs := map[string]Job{}

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		s := strings.Split(scanner.Text(), " ")

		if len(s) == 2 {
			n, err := strconv.Atoi(s[1])
			panicOnErr(err)

			results[s[0][:len(s[0])-1]] = n
		} else {
			jobs[s[0][:len(s[0])-1]] = Job{
				monkey1: s[1],
				op:      s[2],
				monkey2: s[3],
			}
		}
	}
	panicOnErr(scanner.Err())

	s1 = resolve(jobs, results, "root")
	s2 = findHumn(jobs, results)

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
