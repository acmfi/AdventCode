package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	x int
	y int
}

var M = map[byte]Point{
	'L': {-1, 0},
	'R': {1, 0},
	'U': {0, -1},
	'D': {0, 1},
}

func points(data []string) map[Point]int {
	x, y := 0, 0
	c := 0
	p := map[Point]int{}

	for _, m := range data {
		for i, _ := strconv.Atoi(m[1:]); i > 0; i-- {
			x += M[m[0]].x
			y += M[m[0]].y
			c++

			if _, ok := p[Point{x, y}]; !ok {
				p[Point{x, y}] = c
			}
		}
	}

	return p
}

func solve(path1 []string, path2 []string) (int, int) {
	p1 := points(path1)
	p2 := points(path2)

	dist := 0
	steps := 0

	for p := range p1 {
		if _, ok := p2[p]; ok {
			/* Part 1 */
			d := int(math.Abs(float64(p.x)) + math.Abs(float64(p.y)))
			if dist == 0 || d < dist {
				dist = d
			}

			/* Part 2 */
			s := p1[p] + p2[p]
			if steps == 0 || s < steps {
				steps = s
			}
		}
	}

	return dist, steps
}

func main() {
	file, err := os.Open(os.Args[1])
	if err != nil {
		panic(err)
	}
	defer file.Close()

	paths := [][]string{}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		paths = append(paths, strings.Split(scanner.Text(), ","))
	}

	part1, part2 := solve(paths[0], paths[1])

	fmt.Printf("Part 1: %d\n", part1)
	fmt.Printf("Part 2: %d\n", part2)
}
