package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

func main() {
	file, _ := ioutil.ReadFile(os.Args[1])
	input := strings.Split((string(file)), "\n\n")
	// Part 1
	countGroupResponses := 0
	for _, customsAnswer := range input {
		groupResponse := strings.Replace(customsAnswer, "\n", "", -1)
		yesAnswer := ""
		for i := 0; i < len(groupResponse); i++ {
			if !strings.Contains(yesAnswer, string(groupResponse[i])) {
				yesAnswer = yesAnswer + string(groupResponse[i])
			}
			if i == len(groupResponse)-1 {
				countGroupResponses = countGroupResponses + len(yesAnswer)
			}
		}
	}
	// Part 2
	totalAnswers := 0
	for _, group := range input {
		groupAnswers := make(map[string]int)
		people := strings.Split(group, "\n")
		for _, person := range people {
			for _, question := range person {
				groupAnswers[string(question)]++
			}
		}
		groupTotal := 0
		for _, answerCount := range groupAnswers {
			if answerCount == len(people) {
				groupTotal++
			}
		}
		totalAnswers += groupTotal
	}
	fmt.Printf("Day 6\nPart 1: %d\nPart 2: %d\n", countGroupResponses, totalAnswers)
}
