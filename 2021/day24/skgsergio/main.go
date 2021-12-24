package main

import (
	"flag"
	"fmt"
	"math"
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

type Range struct {
	from int
	to   int
	step int
}

func NewRange(from int, to int, step int) *Range {
	if to > from && step <= 0 {
		panic("to is greater than from but step is decremental")
	}

	if to < from && step >= 0 {
		panic("to is lower than from but step is incremental")
	}

	return &Range{from: from, to: to, step: step}
}

func (r *Range) Iter() chan int {
	c := make(chan int)
	go func() {
		for i := r.from; (r.step > 0 && i < r.to) || (r.step < 0 && i > r.to); i += r.step {
			c <- i
		}
		close(c)
	}()
	return c
}

type Inst struct {
	op  string
	dst string
	arg string
}

func (i *Inst) ArgToInt() int {
	n, err := strconv.Atoi(i.arg)
	panicOnErr(err)
	return n
}

func eval(block []Inst, n int, z int) int {
	x := 1
	if (z%26)+block[5].ArgToInt() == n {
		x = 0
	}
	return ((z / block[4].ArgToInt()) * ((25 * x) + 1)) + (x * (n + block[15].ArgToInt()))
}

func find(searchRange *Range, prog []Inst, pc int, bc int, z int) int {
	// Last block
	if pc+18 >= len(prog) {
		for i := range searchRange.Iter() {
			if eval(prog[pc:], i, z) == 0 {
				return i
			}
		}
	} else {
		if prog[pc+4].arg == "1" {
			for i := range searchRange.Iter() {
				res := find(searchRange, prog, pc+18, bc-1, eval(prog[pc:pc+18], i, z))
				if res > 0 {
					return (i * int(math.Pow10(bc))) + res
				}
			}
		} else {
			x := (z % 26) + prog[pc+5].ArgToInt()
			if x <= 0 || x > 9 {
				return 0
			}

			res := find(searchRange, prog, pc+18, bc-1, eval(prog[pc:pc+18], x, z))
			if res > 0 {
				return (x * int(math.Pow10(bc))) + res
			}
		}
	}

	return 0
}

func solve(prog []Inst) (int, int) {
	return find(NewRange(9, 0, -1), prog, 0, 13, 0), find(NewRange(1, 10, 1), prog, 0, 13, 0)
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
	prog := []Inst{}

	for _, line := range strings.Split(strings.TrimSpace(string(input)), "\n") {
		f := strings.Fields(line)

		i := Inst{op: f[0], dst: f[1]}
		if len(f) == 3 {
			i.arg = f[2]
		}

		prog = append(prog, i)
	}

	// Solve
	star1, star2 := solve(prog)
	fmt.Printf("Star 1: %d\n", star1)
	fmt.Printf("Star 2: %d\n", star2)
}
