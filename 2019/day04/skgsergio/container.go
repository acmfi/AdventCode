package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

func byteCount(bytes []byte) map[byte]int {
	count := map[byte]int{}

	for _, b := range bytes {
		count[b] += 1
	}

	return count
}

func sortedString(str string) string {
	b := []byte(str)

	sort.Slice(b, func(i int, j int) bool { return b[i] < b[j] })

	return string(b)
}

func solve(init int, end int) (int, int) {
	p1c := 0
	p2c := 0

	for i := init; i <= end; i++ {
		n := strconv.Itoa(i)
		ns := sortedString(n)

		if n == ns {
			counts := byteCount([]byte(ns))
			/* Part 1 */
			for _, v := range counts {
				if v >= 2 {
					p1c++
					break
				}
			}

			/* Part 2 */
			for _, v := range counts {
				if v == 2 {
					p2c++
					break
				}
			}
		}
	}

	return p1c, p2c
}

func main() {
	input := strings.Split(os.Args[1], "-")
	init, err := strconv.Atoi(input[0])
	if err != nil {
		panic(err)
	}

	end, err := strconv.Atoi(input[1])
	if err != nil {
		panic(err)
	}

	part1, part2 := solve(init, end)

	fmt.Printf("Part 1: %d\n", part1)
	fmt.Printf("Part 2: %d\n", part2)
}
