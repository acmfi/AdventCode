package main

import (
	"bufio"
	"errors"
	"fmt"
	"log"
	"os"
	"reflect"
	"strconv"
	"strings"
)

const (
	centralPortX = 0
	centralPortY = 0
)

type point struct {
	x, y int
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func (p *point) String() string {
	return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func (p *point) manhattan(q *point) int {
	return (abs(p.x-q.x) + abs(p.y-q.y))
}

func expandWire(path []string) []*point {
	var expanded []*point
	last := &point{x: centralPortX, y: centralPortY}
	for _, p := range path {
		direction := string(p[0])
		length, err := strconv.Atoi(p[1:])
		if err != nil {
			log.Fatal(err)
		}
		x := last.x
		y := last.y
		switch direction {
		case "U":
			for i := 1; i <= length; i++ {
				point := point{x: x, y: y + i}
				expanded = append(expanded, &point)
			}
		case "D":
			for i := 1; i <= length; i++ {
				point := point{x: x, y: y - i}
				expanded = append(expanded, &point)
			}
		case "R":
			for j := 1; j <= length; j++ {
				point := point{x: x + j, y: y}
				expanded = append(expanded, &point)
			}
		case "L":
			for j := 1; j <= length; j++ {
				point := point{x: x - j, y: y}
				expanded = append(expanded, &point)
			}
		}
		last = expanded[len(expanded)-1]
	}
	return expanded
}
func main() {
	file, err := os.Open("input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	wires := make([][]string, 0)
	for scanner.Scan() {
		line := scanner.Text()
		path := strings.Split(line, ",")
		wires = append(wires, path)
	}
	wire1 := expandWire(wires[0])
	wire2 := expandWire(wires[1])
	centralPort := &point{x: centralPortX, y: centralPortY}
	distances := make([]int, 0)
	for i, p := range wire1 {
		for j, q := range wire2 {
			if reflect.DeepEqual(p, q) {
				fmt.Printf("intersection: %s\ndistance from central port %s: %d\n", p, centralPort, centralPort.manhattan(p))
				steps1 := 0
				steps2 := 0
				for u := range wire1[:i+1] {
					steps1 += wire1[u].manhattan(wire1[u+1])
				}
				for v := range wire2[:j+1] {
					steps2 += wire2[v].manhattan(wire2[v+1])
				}
				fmt.Printf("steps wire1: %d, steps wire2: %d\n", steps1, steps2)
				fmt.Printf("sum: %d\n", steps1+steps2)
				fmt.Println()
				distances = append(distances, steps1+steps2)
			}
		}
	}

	shortest, err := min(distances)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("answer: %d\n", shortest)
}

func min(a []int) (int, error) {
	if len(a) == 0 {
		return 0, errors.New("empty slice")
	}
	min := a[0]
	for i := range a {
		if a[i] < min {
			min = a[i]
		}
	}
	return min, nil
}
