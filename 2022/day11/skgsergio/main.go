package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
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

type Monkey struct {
	Items     []int
	Operation string
	OpValue   int
	IfDivisor int
	IfTrue    int
	IfFalse   int
}

func inspect(monkeys []Monkey, rounds int, wlFn func(int) int) int {
	inspections := make([]int, len(monkeys))

	monkeysCpy := make([]Monkey, len(monkeys))
	copy(monkeysCpy, monkeys)

	for i := 0; i < rounds; i++ {
		for mIdx, monkey := range monkeysCpy {
			for _, wl := range monkey.Items {
				inspections[mIdx]++

				switch monkey.Operation {
				case "+":
					wl += monkey.OpValue
				case "*":
					wl *= monkey.OpValue
				case "^":
					for p := 1; p < monkey.OpValue; p++ {
						wl *= wl
					}
				}

				wl = wlFn(wl)

				tIdx := monkey.IfFalse
				if wl%monkey.IfDivisor == 0 {
					tIdx = monkey.IfTrue
				}

				monkeysCpy[tIdx].Items = append(monkeysCpy[tIdx].Items, wl)
			}

			monkeysCpy[mIdx].Items = []int{}
		}
	}

	sort.Sort(sort.Reverse(sort.IntSlice(inspections)))

	return inspections[0] * inspections[1]
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	inputBytes, err := ioutil.ReadAll(input)
	panicOnErr(err)

	// old * old == old ^ 2, scanf stops in spaces... remove spaces from item list
	inputStr := strings.NewReplacer("* old", "^ 2", ", ", ",").Replace(string(inputBytes))

	monkeysStr := strings.Split(strings.TrimSpace(inputStr), "\n\n")

	monkeys := make([]Monkey, len(monkeysStr))
	wld := 1
	for _, mStr := range monkeysStr {
		var idx int
		var itemList string
		var monkey Monkey

		fmt.Sscanf(
			mStr,
			`Monkey %d:
  Starting items: %s
  Operation: new = old %s %d
  Test: divisible by %d
    If true: throw to monkey %d
    If false: throw to monkey %d`,
			&idx,
			&itemList,
			&monkey.Operation, &monkey.OpValue,
			&monkey.IfDivisor,
			&monkey.IfTrue,
			&monkey.IfFalse,
		)

		for _, item := range strings.Split(itemList, ",") {
			itemVal, err := strconv.Atoi(item)
			panicOnErr(err)
			monkey.Items = append(monkey.Items, itemVal)
		}

		wld *= monkey.IfDivisor

		monkeys[idx] = monkey
	}

	s1 = inspect(monkeys, 20, func(wl int) int { return wl / 3 })
	s2 = inspect(monkeys, 10000, func(wl int) int { return wl % wld })

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
