package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"strconv"
)

var (
	inputFile = flag.String("f", "input.txt", "Puzzle input file")
)

func exitOnError(err error) {
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}

func readLines(path string) []string {
	file, err := os.Open(path)
	exitOnError(err)
	defer func() {
		err := file.Close()
		exitOnError(err)
	}()

	scanner := bufio.NewScanner(file)

	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}
	return lines
}

type Tree struct {
	x int
	y int
}

type TreeSpecs struct {
	height      int
	scenicScore int
}

func parseTreeMap(input []string) map[Tree]TreeSpecs {
	treeMap := make(map[Tree]TreeSpecs)
	x := 0
	for _, line := range input {
		y := 0
		for _, tree := range line {
			treeHeight, err := strconv.Atoi(string(tree))
			exitOnError(err)
			treeMap[Tree{
				x: x,
				y: y,
			}] = TreeSpecs{height: treeHeight, scenicScore: 0}
			y++
		}
		x++
	}
	return treeMap
}

func solve(treeMap map[Tree]TreeSpecs, maxLength, maxWidth int) (int, int) {
	// Edge trees are always visible
	countVisible := (maxLength * 2) + ((maxWidth - 2) * 2)

	// Part 1
	for x := 1; x < maxWidth-1; x++ {
		for y := 1; y < maxLength-1; y++ {
			tree := Tree{
				x: x,
				y: y,
			}
			// Check Tree visibility
			visible, scenicScore := checkTreeVisibility(treeMap, tree, maxLength, maxWidth)
			if visible {
				countVisible++
			}
			// Update the Tree Scenic Score
			treeMap[tree] = TreeSpecs{height: treeMap[tree].height, scenicScore: scenicScore}
		}

	}
	// Part 2
	scenicScore := 0
	for _, tree := range treeMap {
		if tree.scenicScore > scenicScore {
			scenicScore = tree.scenicScore
		}
	}
	return countVisible, scenicScore
}

func main() {
	// Parse Flags
	flag.Parse()
	// Read Input file
	input := readLines(*inputFile)

	// Forest length and width
	maxLength := len(input)
	maxWidth := len(input[0])

	// Parse tree map
	treeMap := parseTreeMap(input)

	// Solve
	part1, part2 := solve(treeMap, maxLength, maxWidth)

	fmt.Printf("Part 1 - Result: %d\n", part1)
	fmt.Printf("Part 2 - Result: %d\n", part2)
}

func checkTreeVisibility(treeMap map[Tree]TreeSpecs, tree Tree, maxLength, maxWidth int) (bool, int) {
	// Left
	leftVisibility := true
	stepsLeft := 0
	for y := tree.y - 1; y >= 0; y-- {
		stepsLeft++
		// If a tree in the path has the same or more height
		// there is no visibility in this path
		if treeMap[Tree{
			x: tree.x,
			y: y,
		}].height >= treeMap[tree].height {
			leftVisibility = false
			break
		}
	}

	// Right
	rightVisibility := true
	stepsRight := 0
	for y := tree.y + 1; y < maxWidth; y++ {
		stepsRight++

		// If a tree in the path has the same or more height
		// there is no visibility in this path
		if treeMap[Tree{
			x: tree.x,
			y: y,
		}].height >= treeMap[tree].height {
			rightVisibility = false
			break
		}
	}

	// Down
	downVisibility := true
	stepsDown := 0
	for x := tree.x + 1; x < maxLength; x++ {
		stepsDown++

		// If a tree in the path has the same or more height
		// there is no visibility in this path
		if treeMap[Tree{
			x: x,
			y: tree.y,
		}].height >= treeMap[tree].height {
			downVisibility = false
			break
		}
	}

	// Up
	upVisibility := true
	stepsUp := 0
	for x := tree.x - 1; x >= 0; x-- {
		stepsUp++

		// If a tree in the path has the same or more height
		// there is no visibility in this path
		if treeMap[Tree{
			x: x,
			y: tree.y,
		}].height >= treeMap[tree].height {
			upVisibility = false
			break
		}
	}

	// Part 2
	scenicScore := stepsLeft * stepsRight * stepsDown * stepsUp

	// Part 1
	if leftVisibility || rightVisibility || upVisibility || downVisibility {
		return true, scenicScore
	}

	return false, scenicScore
}
