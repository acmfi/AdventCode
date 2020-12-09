package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

/*
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
*/
func main() {
	file, _ := ioutil.ReadFile(os.Args[1])
	input := strings.Split((string(file)), "\n")
	bagsList := make(map[string]map[string]int)
	for _, value := range input {
		replacer := strings.NewReplacer(" contain", "", " bags", "", " bag", "", ",", "", ".", "")
		elements := replacer.Replace(string(value))
		bags := strings.Split(elements, " ")

		initBag := bags[0] + bags[1]

		entryBag := make(map[string]int)
		for i := 0; i < len(bags[2:])-2; i = i + 3 {
			items := bags[2:][i]
			bagType := bags[2:][i+1] + bags[2:][i+2]
			entryBag[bagType], _ = strconv.Atoi(items)
		}
		bagsList[initBag] = entryBag
	}

	// How many bag colors can eventually contain at least one shiny gold bag?
	fmt.Printf("Day 7\nPart 1: %d\n", len(countBagType(bagsList, "shinygold")))
}

func countBagType(bagsList map[string]map[string]int, bagType string) map[string]bool {
	// check contains if each color has shinygold within
	check := make(map[string]bool)
	// iterate over bagsList
	for parent, children := range bagsList {
		// iterate over childrens
		for childs := range children {
			// for each child if their type == shinygold
			if childs == bagType {
				// mark parent as check to not iterate anymore
				check[parent] = true
				for parentsParent := range countBagType(bagsList, parent) {
					check[parentsParent] = true
				}
			}
		}
	}
	return check
}
