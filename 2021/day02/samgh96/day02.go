package main

import (
        "fmt"
        "log"
        "os"
        "strconv"
        "strings"
)

func star1(command []string, amount []int) {
        horizontal := 0
        depth := 0
        for i, s := range command {
                if s == "up" {
                        depth -= amount[i]
                }

                if s == "down" {
                        depth += amount[i]
                }

                if s == "forward" {
                        horizontal += amount[i]
                }
        }

        fmt.Printf("star 1: %d\n", horizontal * depth)
}

func star2(command []string, amount []int) {
	horizontal := 0
	depth := 0
	aim := 0
	for i, s := range command {
                if s == "up" {
                        aim -= amount[i]
                }

                if s == "down" {
                        aim += amount[i]
                }

                if s == "forward" {
                        horizontal += amount[i]
			depth += aim * amount[i]
                }
        }

	fmt.Printf("star 2: %d\n", horizontal * depth)
}

func main() {
        rawInput, err := os.ReadFile(os.Args[1])
        if err != nil {
                log.Fatal(err)
                return
        }

        parsedCommands := []string{}
        parsedUnits := []int{}
        for _, str := range strings.Split(string(rawInput), "\n") {
                if str != "" {
                        splittedStr := strings.Split(str, " ")

                        splittedInt, err := strconv.Atoi(splittedStr[1])
                        if err != nil {
                                log.Fatal(err)
                                return
                        }

                        parsedCommands = append(parsedCommands, splittedStr[0])
                        parsedUnits = append(parsedUnits, splittedInt)
                }
        }

        star1(parsedCommands, parsedUnits)
        star2(parsedCommands, parsedUnits)
}
