package main

import (
	"bufio"
	"flag"
	"fmt"
	"math"
	"os"

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

func min(a int, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a int, b int) int {
	if a > b {
		return a
	}
	return b
}

type Material int

const (
	Ore Material = iota
	Clay
	Obsidian
	Geode
)

type Materials [4]int

type Bots Materials

type BotCost [4]Materials

type Blueprint struct {
	number  int
	botCost BotCost
	maxCost Materials
}

type Status struct {
	time       int
	bots       Bots
	production Materials
}

func calcOpenGeodes(blueprint Blueprint, timeRemaining int, bots Bots, stock Materials) int {
	openGeodes := stock[Geode] + (bots[Geode] * timeRemaining)

	for botType, botCost := range blueprint.botCost {
		if Material(botType) != Geode && bots[botType] >= blueprint.maxCost[botType] {
			continue
		}

		requiredRime := 0
		canBuild := true

		for matType, matQty := range botCost {
			if matQty == 0 {
				continue
			}

			if bots[matType] == 0 {
				canBuild = false
				break
			}

			requiredRime = max(requiredRime, -(int(math.Floor(float64(stock[matType]-matQty) / float64(bots[matType])))))
		}

		if !canBuild {
			continue
		}

		newTimeRemaining := timeRemaining - requiredRime - 1
		if newTimeRemaining <= 0 {
			continue
		}

		newBots := bots
		newBots[botType] += 1

		newStock := stock
		for matType, matQty := range botCost {
			newStock[matType] -= matQty
		}

		for mat := range newStock {
			newStock[mat] += bots[mat] * (requiredRime + 1)

			if Material(mat) != Geode {
				newStock[mat] = min(newStock[mat], blueprint.maxCost[mat]*newTimeRemaining)
			}
		}

		openGeodes = max(openGeodes, calcOpenGeodes(blueprint, newTimeRemaining, newBots, newStock))
	}

	return openGeodes
}

func solve(input *os.File) (int, int) {
	s1 := 0
	s2 := 0

	blueprints := []Blueprint{}

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		b := Blueprint{}

		fmt.Sscanf(
			scanner.Text(),
			`Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian`,
			&b.number,
			&b.botCost[Ore][Ore],
			&b.botCost[Clay][Ore],
			&b.botCost[Obsidian][Ore], &b.botCost[Obsidian][Clay],
			&b.botCost[Geode][Ore], &b.botCost[Geode][Obsidian],
		)

		b.maxCost = Materials{0, b.botCost[Obsidian][Clay], b.botCost[Geode][Obsidian], 0}
		for _, cost := range b.botCost {
			b.maxCost[Ore] = max(b.maxCost[Ore], cost[Ore])
		}

		blueprints = append(blueprints, b)
	}
	panicOnErr(scanner.Err())

	// Part 1
	for _, b := range blueprints {
		s1 += b.number * calcOpenGeodes(b, 24, Bots{1, 0, 0, 0}, Materials{0, 0, 0, 0})
	}

	// Part 2
	s2 = 1
	for _, b := range blueprints {
		if b.number > 3 {
			continue
		}

		s2 *= calcOpenGeodes(b, 32, Bots{1, 0, 0, 0}, Materials{0, 0, 0, 0})
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
