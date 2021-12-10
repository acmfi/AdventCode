package main

import (
        "fmt"
        "log"
        "os"
	"sort"
        "strings"
)

var pairs = map[string]string {
	")": "(",
	"]": "[",
	"}": "{",
	">": "<",
}

var inversePairs = map[string]string {
	"(": ")",
	"[": "]",
	"{": "}",
	"<": ">",
}

var delimiterScores = map[string]int {
        ")": 3,
        "]": 57,
        "}": 1197,
        ">": 25137,
}

var completionScores = map[rune]int {
	')': 1,
        ']': 2,
        '}': 3,
        '>': 4,
}

func repair(line string) string {
	stack := []string{}

	for _, ch := range line {
		c := string(ch)

		switch c {
                case "(", "[", "{", "<":
                        stack = append(stack, c)

		case ")", "]", "}", ">":
			stack = stack[:len(stack) - 1]

		default:
			continue
		}
	}

	res := []string{}
	for i := len(stack) - 1; i >= 0; i-- {
		switch remaining := stack[i]; remaining {
		case "(", "[", "{", "<":
                        res = append(res, inversePairs[remaining])
			
		case ")", "]", "}", ">":
			stack = stack[:len(stack) - 1]
			
		default:
			continue
		}
	}
	
	return strings.Join(res, "")
}

func corrupted(line string) string {
	stack := []string{}

        for _, ch := range line {
		c := string(ch)
		
                switch c {
                case "(", "[", "{", "<":
			
                        stack = append(stack, c)

		case ")", "]", "}", ">":
			
			if stack[len(stack) - 1] != pairs[c] {
				return c
			} else {
				stack = stack[:len(stack) - 1]
			}
			
                default:
                        continue
                }
        }

        return ""
}

func completionScore(s string) int {
	totalScore := 0

	for _, del := range s {
		totalScore *= 5
		totalScore += completionScores[del]
	}
	
	return totalScore
}

func star2(lines []string) {
	completedLines := []string{}
	for _, line := range lines {
		completedLines = append(completedLines, repair(line))
	}

	scores := []int{}
	for _, line := range completedLines {
		scores = append(scores, completionScore(line))
	}

	sort.Ints(scores)

	fmt.Printf("star 2: %d\n", scores[len(scores) / 2])
}

func star1(lines []string) {
	corruptedDelimiters := []string{}
	incompleteLines := []string{}
	
	for _, line := range lines {
		del := corrupted(line)
		
		if del != "" {
			corruptedDelimiters = append(corruptedDelimiters, del)
		} else {
			incompleteLines = append(incompleteLines, line)
		}
	}

	fmt.Println(corruptedDelimiters)
	
	sum := 0
	for _, del := range corruptedDelimiters {
		sum += delimiterScores[del]
	}

	fmt.Printf("star 1: %d\n", sum)

	star2(incompleteLines)
}

func main() {
        rawInput, err := os.ReadFile(os.Args[1])
        if err != nil {
                log.Fatal(err)
                return
        }

        strInput := strings.Fields(string(rawInput))

        star1(strInput)
}
