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

type Node struct {
	value int
	left  *Node
	right *Node
}

func (n *Node) IsLeaf() bool {
	return n.left == nil && n.right == nil
}

func (n *Node) IsPair() bool {
	return !n.IsLeaf() && n.left.IsLeaf() && n.right.IsLeaf()
}

func parse(line string, chr int) (*Node, int) {
	if line[chr] == '[' {
		l, chr := parse(line, chr+1)
		r, chr := parse(line, chr+1)

		return &Node{left: l, right: r}, chr + 1 // +1 to skip , and ]
	}

	v, err := strconv.Atoi(string(line[chr]))
	panicOnErr(err)

	return &Node{value: v}, chr + 1 // +1 to skip , and ]
}

func parseLine(line string) *Node {
	n, _ := parse(line, 0)
	return n
}

func explode(node *Node, level int, done bool, leafNode *Node, addValue int) (bool, *Node, int) {
	if !done && level == 4 && node.IsPair() {
		if leafNode != nil {
			leafNode.value += node.left.value
		}
		addValue = node.right.value

		*node = Node{value: 0}

		return true, nil, addValue
	}

	if node.IsLeaf() {
		node.value += addValue
		return done, node, 0
	}

	done, leafNode, addValue = explode(node.left, level+1, done, leafNode, addValue)
	return explode(node.right, level+1, done, leafNode, addValue)
}

func split(node *Node, done bool) bool {
	if !node.IsLeaf() {
		return split(node.right, split(node.left, done))
	}

	if !done && node.value >= 10 {
		*node = Node{
			left: &Node{
				value: int(math.Floor(float64(node.value) / 2)),
			},
			right: &Node{
				value: int(math.Ceil(float64(node.value) / 2)),
			},
		}

		done = true
	}

	return done
}

func reduce(v *Node) *Node {
	if exploded, _, _ := explode(v, 0, false, nil, 0); !exploded && !split(v, false) {
		return v
	}

	return reduce(v)
}

func magnitude(v *Node) int {
	if v.IsLeaf() {
		return v.value
	}

	return 3*magnitude(v.left) + 2*magnitude(v.right)
}

func star1(lines []string) int {
	var last *Node = nil

	for _, line := range lines {
		if last != nil {
			last = reduce(&Node{left: last, right: parseLine(line)})
		} else {
			last = parseLine(line)
		}
	}

	return magnitude(last)
}

func star2(lines []string) int {
	largest := 0

	for _, line1 := range lines {
		for _, line2 := range lines {
			mag := magnitude(reduce(&Node{left: parseLine(line1), right: parseLine(line2)}))

			if mag > largest {
				largest = mag
			}
		}
	}

	return largest
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
	lines := strings.Split(strings.TrimSpace(string(input)), "\n")

	// Solve
	fmt.Printf("Star 1: %d\n", star1(lines))
	fmt.Printf("Star 2: %d\n", star2(lines))
}
